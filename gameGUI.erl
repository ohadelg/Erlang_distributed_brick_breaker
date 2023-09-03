-module(gameGUI).
-author("Ohad Elgazi").

% upload incluse wxWidget and gameSettings
-include_lib("wx/include/wx.hrl").
% -include("../include/set.hrl").


% Set Behaviour
-behaviour(gen_server).

%---------------------------------------------------------------------------------
% Export Functions

-export([init/1, start_link/0, start/0 , start_monitor/0, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%---------------------------------------------------------------------------------
-define(BALL_DIAMETER, 30).
-define(BLOCK_SPACES, trunc(?BALL_DIAMETER/3)).
-define(BOARD_WIDTH, (1200+?BALL_DIAMETER)).
-define(BOARD_HEIGHT, 600).
-define(BOARD_SIZE, {?BOARD_WIDTH, ?BOARD_HEIGHT}).
-define(BOARD_FRAME_HEIGHT, ?BOARD_HEIGHT+40).
% -define(BOARD_FRAME_WIDTH, ?BOARD_HEIGHT+40).
-define(BOARD_FRAME_SIZE, {?BOARD_WIDTH+5, ?BOARD_FRAME_HEIGHT}).


-define(BALL_IMAGE, "Images/GolfBall.png").

-define(BLUE_BLOCK, "Images/BlueBlock.png").
-define(RED_BLOCK, "Images/RedBlock.png").
-define(BLACK_BLOCK, "Images/BlackBlock.png").
-define(DESIGNED_BLOCK, "Images/DesignedBlock.png").

-define(BLUE_PLATE, "Images/BluePlate.png").
-define(RED_PLATE, "Images/RedPlate.png").
-define(BLACK_PLATE, "Images/BlackPlate.png").
-define(DESIGNED_PLATE, "Images/DesignedPlate.png").

-define(GREY_BORDER, "Images/GreyBorder.png").

-define(LOSS_IMG, "Images/Loss.png").
-define(LOSS_SIZE, 500).

-define(UPDATE_TIME, 300).

% -record(block, {block_id, row, column, xlocation = 50, ylocation = 50, size=60, status=active}).
% -record(plate, {plateID, xlocation, ylocation=50, stepSize=10, plateWidth=300, plateHeight=15, rightGG, leftGG, player}).
% -record(ball, {ballID=1, xlocation=1,ylocation=1, xSpeed=0.5, ySpeed=0.5, ballRadius, player, blockHit=0, popedBlocksNum=0, playerSwitch=0, plateHit=0, wallHit=0}).
% -record(board, {x_size=1000, y_size=500}). % x_size = (numColumns)*blockSpaces, y_size~x_size/2
% -record(generalSet, {numBlocks = 24,
%                      numColumns = 12,
%                      numRows= 2,
%                      blockSpaces = 80,
%                      ballPlateSpeedFactor=3, % BallSpeed/PlateSpeed
%                      boardPlateWidthFactor = 2, % BoardWidth / PlateWidth
%                      ballRadius = 50, 
%                      blocksETSName = blocksETS,
%                      platesETSName = platesETS,
%                      ballETSName = ballETS}).
%---------------------------------------------------------------------------------
% start functions
start()->
    gen_server:start({global, ?MODULE}, ?MODULE, [], []).
start_link()->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
start_monitor()->
    gen_server:start_monitor({global, ?MODULE}, ?MODULE, [], []).
%---------------------------------------------------------------------------------
init([])->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Main Game Frame", [{size, ?BOARD_FRAME_SIZE}]),
    {_WXRef, _Num, _Type, []} = Frame,
    wx_object:set_pid(Frame, self()),
    % MenuBar = wxMenuBar:new(),
    % wxFrame:setMenuBar(Frame, MenuBar),
    % Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    Panel = wxPanel:new(Frame, [{size, {?BOARD_WIDTH, ?BOARD_HEIGHT}}]),
    wxFrame:connect(Panel, paint),
    % wxSizer:add(Sizer, Panel, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}]),
    % wxSizer:setSizeHints(Sizer, Frame),
    wxFrame:show(Frame),
    sketcBall(Panel),
    sketchBorders(Panel),
    sketchBlocks(Panel),
    sketchPlates(Panel),
    erlang:send_after(?UPDATE_TIME, global:whereis_name(gameGUI), updateGUI),
    State = {Panel, Frame, 0},
    {ok, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State,  _Extra) ->
    {ok, State}.

% stop()->
%     gen_server:cast(?MODULE, stop).

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(freeze, {Panel,  _Frame,  _Counter})->
    drawImage(Panel, ?LOSS_IMG, {trunc((?BOARD_WIDTH-?LOSS_SIZE)/2), trunc((?BOARD_HEIGHT-?LOSS_SIZE)/2)}, ?LOSS_SIZE),
    {noreply, freeze};    
handle_cast(stop, State)->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
%-----------------------------------------------------------------------------------------
handle_info(updateGUI, {Panel,  Frame,  Counter})->
    NewPanel = wxPanel:new(Frame, [{size, {?BOARD_WIDTH, ?BOARD_HEIGHT}}]),
    wxPanel:destroy(Panel), 
    wxFrame:connect(NewPanel, paint),
    sketcBall(NewPanel), 
    sketchBorders(NewPanel),
    sketchBlocks(NewPanel),
    sketchPlates(NewPanel),
    % wxFrame:show(Frame),
    erlang:send_after(?UPDATE_TIME, global:whereis_name(gameGUI), updateGUI),
    {noreply, {NewPanel, Frame, Counter+1}};
handle_info(_WXpart, State)->
    {noreply, State}.






%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
% functions
%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
% draw Imege on Panel
%   Input: (Panel::wxPanel(), Imege::string(), {X::integer, Y::integer()}, SIZE::integer() )
drawImage(Panel, ImageSource, Pos, Height)->
    Image = wxImage:new(ImageSource),
    wxImage:rescale(Image, Height, Height),
    ClientDC = wxClientDC:new(Panel),
    Bitmap = wxBitmap:new(Image),
    wxDC:drawBitmap(ClientDC, Bitmap, Pos).

%   Input: (Panel::wxPanel(), Imege::string(), {X::integer, Y::integer()}, Height::integer(), Width::integer())
drawImage(Panel, ImageSource, Pos, Height, Width)->
    Image = wxImage:new(ImageSource),
    wxImage:rescale(Image, Height, Width),
    ClientDC = wxClientDC:new(Panel),
    Bitmap = wxBitmap:new(Image),
    wxDC:drawBitmap(ClientDC, Bitmap, Pos).

%---------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
sketchBorders(Panel)->
    ActivePlayers = getNumPlayer(),
    Delta = ?BOARD_WIDTH/ActivePlayers,
    lists:foreach(fun(X)->
        drawImage(Panel, ?GREY_BORDER, {trunc((X*(Delta))-(3*X)),
                            0},
                            5, ?BOARD_HEIGHT) end,
                                lists:seq(1, ActivePlayers)
                                ),
    ok.

%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
sketchBlocks(Panel)->
    BlocksList = ets:tab2list(blocksETS),
    lists:foreach(fun({_Type, _ID ,_Row, _Column, Xlocation, Ylocation, Size, Status})->
        if 
            Status==active -> 
                drawImage(Panel, ?BLUE_BLOCK, {round(Xlocation), round(Ylocation)}, Size);
            true->
                ok
        end
    end, 
    BlocksList),
    ok.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
sketchPlates(Panel)->
    PlatesList = ets:tab2list(platesETS),
    % {plateID, xlocation, ylocation=50, stepSize=10, plateWidth=150, plateHeight=15, rightGG, leftGG, player}
    lists:foreach(fun({_Type, _ID, Xlocation, Ylocation, _Step, PlateWidth, PlateHieght, _RightGG, _LeftGG, _Player})->
            drawImage(Panel, ?BLACK_PLATE, {round((Xlocation-(0.5*PlateWidth))),
                                     round(?BOARD_HEIGHT-(Ylocation-(0.5*PlateHieght)))},
                                     PlateWidth, PlateHieght)
    end, 
    PlatesList),
    ok.
sketcBall(Panel)->
    BallList = ets:tab2list(ballETS),
    % {ballID=1, xlocation=1,ylocation=1, xSpeed=0.5, ySpeed=0.5, ballRadius=?BALL_DIAMETER, player, blockHit=0, popedBlocksNum=0, playerSwitch=0, plateHit=0, wallHit=0}
    lists:foreach(fun({_Type, _ID, Xlocation, Ylocation, _XSpeed, _YSpeed, _BallDiameter, _Player, _BHIT, _POPED, _PlayerS, _playerHit, _WallHit})->
            drawImage(Panel, ?BALL_IMAGE, {round((Xlocation)-((?BALL_DIAMETER)/2)),
                                     round(Ylocation- ((?BALL_DIAMETER)/2))},
                                     ?BALL_DIAMETER)
    end, 
    BallList),
    ok.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% get_active_players check which players are activated - return list
get_active_players()->
    RegisteredProc = global:registered_names(),
    [X||X<-RegisteredProc, lists:member(X, [player1, player2, player3, player4])].
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% getNumPlayers() = return Number of active players
getNumPlayer()->
    length(get_active_players()).
%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
%---------------------------------------------------------------------------------
