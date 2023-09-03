-module(main).
-author("Ohad Elgazi").
%------------------------------------------------------------------------------
% Set behaviour
-behaviour(gen_server).
-import(gameGUI, [start_link/0]).
%------------------------------------------------------------------------------
% export functions
%------------------------------------------------------------------------------
% export start function
-export([start/1, start_link/1, start_monitor/1, start_game/0]).
% export Mandatory functions
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, stop/0, handle_info/2]).
% export functions
-export([getElemData/0, blockInfo/1]).
%------------------------------------------------------------------------------
% import wx library
% -include("set.hrl"). 
% upload incluse wxWidget and gameSettings
-include_lib("wx/include/wx.hrl").
-define(BALL_DIAMETER, 30).
-define(BLOCK_SPACES, trunc(?BALL_DIAMETER/3)).
-define(BOARD_WIDTH, 1200+?BALL_DIAMETER).
-define(BOARD_HEIGHT, 600).
-define(BOARD_SIZE, {?BOARD_WIDTH, ?BOARD_HEIGHT}).
-define(NUM_OF_BLOCKS, 24).
-define(NUM_OF_COLUMNS, 12).
-define(NUM_OF_ROWS, 2).
-define(PLATE_STEP_SIZE, 1).
-define(BALL_PLATE_SPEED_FACTOR, 1).
-define(BOARD_PLATE_WIDTH_FACTOR, 3).
-define(BLOCKS_ETS, blocksETS).
-define(PLATES_ETS, platesETS).
-define(BALL_ETS, ballETS).
-define(PLATE_WIDTH, trunc(?BOARD_WIDTH/4)).
-define(Plate_HEIGHT, trunc(?PLATE_WIDTH/18)).
-define(BLOCK_DEFAULT_SIZE, trunc((((?BOARD_WIDTH)-(?BLOCK_SPACES))/?NUM_OF_COLUMNS)-?BLOCK_SPACES)).
-define(BLOCKS_CENTERED_SPACES, ?BLOCK_SPACES+?BLOCK_DEFAULT_SIZE).
-define(BLOCK_DEFAULT_X, ?BLOCK_SPACES).
-define(BLOCK_DEFAULT_Y, ?BLOCK_SPACES).
-define(PLAYER1_NODE, 'com1@LAPTOP-A8NM9S74'). % Most to be change
-define(PLAYER2_NODE, 'com2@LAPTOP-A8NM9S74'). % Most to be change if not nessesary set as undefined
-define(PLAYER3_NODE, 'com3@LAPTOP-A8NM9S74'). % Most to be change if not nessesary set as undefined
-define(PLAYER4_NODE, 'com4@LAPTOP-A8NM9S74'). % Most to be change if not nessesary set as undefined
-define(PLAYER_LIST, [{player1, ?PLAYER1_NODE}, {player2, ?PLAYER2_NODE}, {player3, ?PLAYER3_NODE}, {player4, ?PLAYER4_NODE}]).
-define(TIME_SAC2, 10).
%------------------------------------------------------------------------------
-record(block, {block_id, row, column, xlocation =?BLOCK_DEFAULT_X, ylocation =?BLOCK_DEFAULT_Y, size=?BLOCK_DEFAULT_SIZE, status=active}).
-record(plate, {plateID, xlocation, ylocation=50, stepSize=?PLATE_STEP_SIZE, plateWidth=?PLATE_WIDTH, plateHeight= ?Plate_HEIGHT, rightGG, leftGG, player}).
-record(ball, {ballID=1, xlocation=1,ylocation=1, xSpeed=0.5, ySpeed=0.5, ballRadius=?BALL_DIAMETER, player, blockHit=0, popedBlocksNum=0, playerSwitch=0, plateHit=0, wallHit=0}).
% -record(board, {x_size=?BOARD_WIDTH, y_size=?BOARD_HEIGHT}). % x_size = (numColumns)*blockSpaces, y_size~x_size/2
-record(generalSet, {numBlocks = ?NUM_OF_BLOCKS,
                     numColumns = ?NUM_OF_COLUMNS,
                     numRows= ?NUM_OF_ROWS,
                     blockSpaces = ?BLOCKS_CENTERED_SPACES,
                     ballPlateSpeedFactor = ?BALL_PLATE_SPEED_FACTOR, % BallSpeed/PlateSpeed
                     boardPlateWidthFactor = ?BOARD_PLATE_WIDTH_FACTOR, % BoardWidth / PlateWidth
                     ballRadius = ?BALL_DIAMETER, 
                     blocksETSName = ?BLOCKS_ETS,
                     platesETSName = ?PLATES_ETS,
                     ballETSName = ?BALL_ETS}).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
start_game()->
    spawn(main, start, [[]]).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% Start Function
start([])->
    gen_server:start({global, ?MODULE}, ?MODULE, [], []).
start_link([])->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
start_monitor([])->
    gen_server:start_monitor({global, ?MODULE}, ?MODULE, [], []).

% Mandatory callback functions
init([])->
    lists:foreach(fun({_ID, Node})->
            net_adm:ping(Node)
             end, [{D,N}||{D,N}<-?PLAYER_LIST, N=/=undefined]),
    lists:foreach(fun({_ID, Node})->
            rpc:block_call(Node, player_sup, start_link, []),
            wait(1000)
             end, [{D,N}||{D,N}<-?PLAYER_LIST, N=/=undefined]), 
    DataTables = getElemData(),
    setAllETS(DataTables),
    % %--------------------------------
    % % Start GUI
    main_sup:start_link(),
    ets:setopts(ballETS, {heir, whereis(mainSuper), {}}),
    ets:setopts(platesETS, {heir, whereis(mainSuper), {}}),
    ets:setopts(blocksETS, {heir, whereis(mainSuper), {}}),
    %---------------------------------
    erlang:send_after(1000, self(), init_plates),
    {ok, {set_plates}}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop()->
    gen_server:cast(?MODULE, stop).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
handle_call({update_elem, ball, NewBall}, _From, {wait, Stats})->
    BallX = element(#ball.xlocation, NewBall),
    BallY = element(#ball.ylocation, NewBall),
    BallXSpeed = element(#ball.xSpeed, NewBall),
    BallYSpeed = element(#ball.ySpeed, NewBall),
    BallRadius = element(#ball.ballRadius, NewBall),
    BallName = lists:concat(ball, element(#ball.ballID, NewBall)),
    checkBlocksInfluence(ets:tab2list(blocksETS), {BallX, BallY, BallXSpeed, BallYSpeed, BallRadius}, {BallName}),
    ets:insert(ballETS, NewBall),
    Reply = ok,
    {reply, Reply, {wait, Stats}};
handle_call({ball_change_player, _Ball_Info, _NextPlayer}, _From, {wait, Stats})->
    %start_new_Ball(NextPlayer, Ball_Info),
    {reply, stop, {wait, Stats}};
handle_call({update_block, Block_Info}, _From, {wait, Stats})->
    ets:insert(?BLOCKS_ETS, Block_Info),
    Reply = ok,
    {reply, Reply, {wait, Stats}};
handle_call({update_plate, Plate_Info}, _From, {wait, Stats})->
    ets:insert(?PLATES_ETS, Plate_Info),
    Reply = ok,
    {reply, Reply, {wait, Stats}};
handle_call(ball_info_req, _From, State)->
    [{_ball, BallID, Xlocation, Ylocation, XSpeed, YSpeed,
         BallRadius, _Player, _BlockHit, _PopedBlocksNum, _PlayerSwitch,
          _PlateHit, _WallHit}] = ets:tab2list(ballETS),
    ReplyMsg = {ball_info, {list_to_atom(lists:concat([ball, BallID])), Xlocation, Ylocation, XSpeed, YSpeed, BallRadius}},
    {reply,ReplyMsg, State};
handle_call(plateInfoPlease, From, State)->
    Plates_List = ets:tab2list(platesETS),
    lists:foreach(fun(
        {_Type, Plate_Num, Xlocation, Ylocation, StepSize, PlateWidth, PlateHeight,
             RightGG, LeftGG, _Player})->
            Platename = list_to_atom(lists:concat([plate, Plate_Num])),
            Defined = global:whereis_name(Platename),
            case Defined of
                undefined->
                    Plate_info = #plate{plateID = Plate_Num, xlocation=Xlocation, ylocation=Ylocation,
                        stepSize= StepSize*20, plateWidth= PlateWidth, plateHeight = PlateHeight,
                        rightGG = RightGG, leftGG = LeftGG, player= list_to_atom(lists:concat([player, Plate_Num]))},
                    erlang:send(From, {plate_info, set, Plate_info});
                _Else->
                    ok
            end 
        end, Plates_List),
    {noreply, ok,  State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
handle_cast({update_elem, ball, NewBall}, State)->
    BallX = element(#ball.xlocation, NewBall),
    BallY = element(#ball.ylocation, NewBall),
    BallXSpeed = element(#ball.xSpeed, NewBall),
    BallYSpeed = element(#ball.ySpeed, NewBall),
    BallRadius = element(#ball.ballRadius, NewBall),
    BallName = list_to_atom(lists:concat([ball, element(#ball.ballID, NewBall)])),
    checkBlocksInfluence(ets:tab2list(blocksETS), {BallX, BallY, BallXSpeed, BallYSpeed, BallRadius}, {BallName}),
    ets:insert(ballETS, NewBall),
    {noreply, State};
handle_cast({update_elem, plate, NewPlate}, State)->
    ets:insert(platesETS, NewPlate),
    {noreply, State};
handle_cast({update_elem, block, NewBall}, State)->
    ets:insert(blocksETS, NewBall),
    {noreply, State};
handle_cast(_Msg, State)->
    {noreply, State}.
%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
handle_info(init_plates, {set_plates}) ->
    Plates_List = ets:tab2list(platesETS),
    lists:foreach(fun(
        {_Type, Plate_Num, Xlocation, Ylocation, StepSize, PlateWidth, PlateHeight,
             RightGG, LeftGG, _Player})->
            Plate_info = #plate{plateID = Plate_Num, xlocation=Xlocation, ylocation=Ylocation,
                 stepSize= StepSize*20, plateWidth= PlateWidth, plateHeight = PlateHeight,
                  rightGG = RightGG, leftGG = LeftGG, player= list_to_atom(lists:concat([player, Plate_Num]))},
           global:send(list_to_atom(lists:concat([plate, Plate_Num])), {plate_info, set, Plate_info}) end, Plates_List),
    erlang:send_after(?TIME_SAC2, self(), init_ball),
    {noreply, {set_balls}};
handle_info(init_ball, {set_balls}) ->
    [Ball] = ets:tab2list(ballETS),
    BallName = list_to_atom(lists:concat([ball, element(2,Ball)])),
    Not_Active_balls = [X||X<-[ball1, ball2, ball3, ball4], global:whereis_name(X)=/=undefined, X=/=BallName],
    % gen_statem:call({global, BallName},
    %                 {ball_info, set, Ball}),
    lists:foreach(fun(X)->global:send(X, {ball_info, not_active}) end, Not_Active_balls),
    global:send(BallName, {ball_info, set,  Ball}),
    {noreply, {}};
handle_info(calculate_step, {})->
    [Ball] = ets:tab2list(ballETS),
    BallOwner = element(#ball.player+1, Ball),
    gen_statem:call({global, BallOwner}, step),
    erlang:send_after(?TIME_SAC2, self(), calculate_step),
    {noreply, {}};
handle_info(_Msg, State)->
    {noreply, State}.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% start getElemData()
getElemData()->   
    BlockList = setBlockList(),
    PlatesList = setPlateList(?BOARD_WIDTH),
    Plate1 = lists:nth(1, PlatesList),
    BallInfo =  setBallInfo(Plate1#plate.xlocation, Plate1#plate.ylocation, Plate1#plate.stepSize, Plate1#plate.player),
    PlayerBoardList =[],
    [BlockList, PlatesList, BallInfo, PlayerBoardList].
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% return List of all blocks and information
setBlockList()->
    % create list of all block settings
    lists:map(fun(Elem)->blockInfo(Elem) end, lists:seq(1,?NUM_OF_BLOCKS)).
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
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% return plates list of all information {plateID, xlocation, ylocation, stepSize=50, plateWidth=150, rightGG, leftGG, player}
setPlateList(BoardSize)->
    NumPlayers = getNumPlayer(),
    PlayerBoardSize = playerBoardSize(BoardSize, NumPlayers),
    PlateWidth = plateWidth(PlayerBoardSize),
    % loop -> function 
    lists:map(fun(Elem)->setPlate(Elem, PlayerBoardSize, PlateWidth) end, lists:seq(1, NumPlayers)).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%setPlate() -> {plateID, xlocation, ylocation, stepSize=50, plateWidth=150, rightGG, leftGG, player}
setPlate(Indx, PlayerBoardSize, PlateWidth)->
    Xlocation = PlayerBoardSize*(Indx-0.5),
    RightGG = (Indx*PlayerBoardSize)-(0.5*PlateWidth),
    LeftGG = ((Indx-1)*PlayerBoardSize)+(0.5*PlateWidth),
    Player = lists:nth(Indx, get_active_players()),
    #plate{plateID=Indx, xlocation=Xlocation, plateWidth=PlateWidth, rightGG=RightGG, leftGG=LeftGG, player=Player}.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% playerBoarSize(#board.x_size)) -> 
playerBoardSize(Boardsize, NumPlayers)->
    try trunc(Boardsize/NumPlayers) of
        _->trunc(Boardsize/NumPlayers)
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% plateWidth(#board.x_size)) -> 
plateWidth(PlayerBoardSize)->
    trunc(PlayerBoardSize/?BOARD_PLATE_WIDTH_FACTOR).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% setBallInfo -> set Init Info for the ball {xlocation=1, ylocation=1, xSpeed=0.5, ySpeed=0.5, ballRadius,  player, blockHit, popedBlocksNum, playerSwitch, plateHit, wallHit}
setBallInfo(PlateXlocation, PlateYlocation, PlateSpeed, Player)->
    Ylocation = PlateYlocation + 40,
    [XSpeed, YSpeed] = ballSpeedSetup(PlateSpeed),
    #ball{xlocation=PlateXlocation, ylocation=(?BOARD_HEIGHT)-Ylocation, xSpeed=XSpeed, ySpeed=YSpeed, ballRadius=element(#generalSet.ballRadius, #generalSet{}), player=Player}.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% ballSpeedSetup(Speed)-> [XSpeed, YSpeed]
ballSpeedSetup(PlateSpeed)->
    SpeedFactor = ?BALL_PLATE_SPEED_FACTOR, % get the difault value for speed Factor
    Speed = PlateSpeed*SpeedFactor,
    RandAngle = rand:uniform()*0.5+0.25,
    RandX = rand:uniform(),
    RandY = rand:uniform(),
    if 
        RandX>=0.5 -> Xside = 1;
        true-> Xside = -1
    end,
    if 
        RandY>=0.5 -> _Yside = 1;
        true-> _Yside = -1
    end,
    XSpeed = Xside*Speed*RandAngle,
    YSpeed = -1*math:sqrt(1-math:pow(RandAngle, 2))*Speed, %Yside*math:sqrt(1-math:pow(RandAngle, 2))*Speed,
    [XSpeed, YSpeed].
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% set block info list
% Example: 
%           1. Block = BlockInfo(NumID),
%           2. {} = blockInfo(NumID)
blockInfo(NumID)->
    case NumID rem ?NUM_OF_COLUMNS of
        0-> % in case last column.
            Column = ?NUM_OF_COLUMNS,
            Row = trunc(NumID/?NUM_OF_COLUMNS);
        _->
            Column = NumID rem ?NUM_OF_COLUMNS,
            Row = trunc(NumID/?NUM_OF_COLUMNS)+1
    end,
    Xlocation = trunc((?BLOCK_DEFAULT_X)+((Column-1)*(?BLOCKS_CENTERED_SPACES))),
    Ylocation = trunc((?BLOCK_DEFAULT_Y)+((Row-1)*(?BLOCKS_CENTERED_SPACES))),
    #block{block_id=NumID, row=Row, column=Column, xlocation =Xlocation , ylocation = Ylocation}.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
setAllETS([BlockList, PlatesList, BallInfo, _PlayerBoardList])->
    newBlockETS(BlockList),
    newPlateETS(PlatesList),
    newBallETS(BallInfo),
    % newPlayerETS(PlayerBoardList),
    ok.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% creat new ETS for Block
newBlockETS(BlockList)->
    ets:new(element(#generalSet.blocksETSName, #generalSet{}), [set, public, named_table, {keypos, 2}]),
    ets:insert(blocksETS, BlockList).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% creat new ETS for Block
newPlateETS(PlatesList)->
    ets:new(element(#generalSet.platesETSName, #generalSet{}), [set, public, named_table, {keypos, 2}]),
    ets:insert(platesETS, PlatesList).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% creat new ETS for Block
newBallETS(BallInfo)->
    ets:new(element(#generalSet.ballETSName, #generalSet{}), [set, public, named_table, {keypos, 2}]),
    ets:insert(ballETS, BallInfo).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
checkBlocksInfluence(ListOfBlocks, {BallX, BallY, BallXSpeed, BallYSpeed, BallRadius}, {BallName})->
    lists:foreach(fun({_Type, Block_id, Row, Column, Xlocation, Ylocation, Size, Status})->
            % Diff = round(0.5*Size),
            if 
                Status == active->
                    {LeftX, RightX, UpperY, LowerY} = {Xlocation, Xlocation+Size, Ylocation, Ylocation+Size},
                    Checked = checkIfInside({LeftX, RightX, UpperY, LowerY}, {BallX, BallY, BallXSpeed, BallYSpeed, BallRadius}),
                    case Checked of
                        {crossed, Action}->
                            Msg = list_to_atom(lists:concat([Action, block])),
                            gen_statem:cast({global, BallName}, Msg), %send Ball Action 
                            NewBlock = #block{block_id=Block_id, row=Row, column=Column, xlocation=Xlocation, ylocation=Ylocation, size=Size, status=breaked},
                            ets:insert(blocksETS, NewBlock);
                        undefined->
                            ok
                    end;
                true->ok
            end
          end, ListOfBlocks).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
checkIfCrossLine({LeftX, RightX, UpperY, LowerY}, {BallX, BallY, BallXSpeed, BallYSpeed, BallRadius})-> %Lower = The Smaller one
    if 
        (((BallX-(BallRadius/2))>=RightX) and ((BallX+BallXSpeed-(BallRadius/2))=<RightX))->
            {true, flipX};
        (((BallX+(BallRadius/2))=<LeftX) and ((BallX+BallXSpeed+(BallRadius/2))>=LeftX))->
            {true, flipX};
        (((BallY+(BallRadius/2))=<LowerY) and ((BallY+BallYSpeed+(BallRadius/2))>=LowerY))->
            {true, flipY};
        (((BallY-(BallRadius/2))>=UpperY) and ((BallY+BallYSpeed-(BallRadius/2))=<LowerY))->
            {true, flipY};
        true->false
    end.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
checkIfInside({LeftX, RightX, UpperY, LowerY}, {BallX, BallY, BallXSpeed, BallYSpeed, BallRadius})-> %Lower = The Smaller one
    CrossedLine = checkIfCrossLine({LeftX, RightX, UpperY, LowerY}, {BallX, BallY, BallXSpeed, BallYSpeed, BallRadius}),
    case CrossedLine of
        {true, Action}->  
            DisCheck =distanceForBlock({BallX, BallY}, {LeftX, RightX, UpperY, LowerY}, (RightX-LeftX),  BallRadius),
            if 
                DisCheck==true->
                        {crossed, Action};
                % (((BallX+BallXSpeed)>=LeftX) and ((BallX+BallXSpeed)=<RightX) and
                %     ((BallY+BallYSpeed)>=LowerY) and ((BallY+BallYSpeed)=<UpperY)) ->
                true->undefined
            end;
        _Else->
            undefined
    end.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
distanceForBlock({BallX, BallY}, {LeftX, RightX, UpperY, LowerY}, Size,  BallRadius)->
    NewSize = ((Size*1.4)+(BallRadius/2)),
    Dis1=distance(BallX, BallY, LeftX, UpperY),
    Dis2=distance(BallX, BallY, LeftX, LowerY),
    Dis3=distance(BallX, BallY, RightX, UpperY),
    Dis4=distance(BallX, BallY, RightX, LowerY),
    if %distance from all points of the block
        (Dis1=<NewSize),
        (Dis2=<NewSize),
        (Dis3=<NewSize),
        (Dis4=<NewSize)->
            true;
        true->false
    end.   
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
distance(X1,Y1,X2,Y2)->
    math:sqrt(math:pow(X1-X2, 2)+math:pow(Y1-Y2, 2)).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
wait(Sec) -> 
   receive
   after (Sec) -> ok
end.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------



