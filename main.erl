-module(main).
-author("Ohad Elgazi").

-author("Ohad Elgazi").

% upload incluse wxWidget and gameSettings
% -include("set.hrl").

% Set behaviour
-behaviour(gen_server).
-import(gameGUI, [start_link/0]).
%------------------------------------------------------------------------------
% export functions
%------------------------------------------------------------------------------
% export start function
-export([start/1, start_link/1, start_monitor/1]).
% export Mandatory functions
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, stop/0, handle_info/2]).
% export functions
-export([getElemData/0, blockInfo/1]).
%------------------------------------------------------------------------------
% import wx library
-include_lib("wx/include/wx.hrl").
-define(BALL_DIAMETER, 30).
-define(BLOCK_SPACES, trunc(?BALL_DIAMETER/3)).

-define(BOARD_WIDTH, 1200+?BALL_DIAMETER).
-define(BOARD_HEIGHT, 600).
-define(BOARD_SIZE, {?BOARD_WIDTH, ?BOARD_HEIGHT}).
-define(NUM_OF_BLOCKS, 24).
-define(NUM_OF_COLUMNS, 12).
-define(NUM_OF_ROWS, 2).
-define(BALL_PLATE_SPEED_FACTOR, 3).
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
-define(PLAYER1_NODE, 'com1@LAPTOP-A8NM9S74').
-define(PLAYER2_NODE, 'com2@LAPTOP-A8NM9S74').
-define(PLAYER3_NODE, 'com3@LAPTOP-A8NM9S74').
-define(PLAYER4_NODE, undefined).
-define(PLAYER_LIST, [{player1, ?PLAYER1_NODE}, {player2, ?PLAYER2_NODE}, {player3, ?PLAYER3_NODE}, {player4, ?PLAYER4_NODE}]).

%------------------------------------------------------------------------------
-record(block, {block_id, row, column, xlocation =?BLOCK_DEFAULT_X, ylocation =?BLOCK_DEFAULT_Y, size=?BLOCK_DEFAULT_SIZE, status=active}).
-record(plate, {plateID, xlocation, ylocation=50, stepSize=10, plateWidth=?PLATE_WIDTH, plateHeight= ?Plate_HEIGHT, rightGG, leftGG, player}).
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
% Start Function
start([])->
    gen_server:start({global, ?MODULE}, ?MODULE, [], []).
start_link([])->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
start_monitor([])->
    gen_server:start_monitor({global, ?MODULE}, ?MODULE, [], []).

% Mandatory callback functions
init([])->
    % io:format("All players: ~w.~n", [[{D,N}||{D,N}<-?PLAYER_LIST, N=/=undefined]]),
    % case ?PLAYER1_NODE of
    %     undefined->ok;
    %     Else->
    %         net_adm:ping(Else),
    %         spawn(Else, player_sup, start_link, [])
    % end,
    % wait(5000),
    % case ?PLAYER2_NODE of
    %     undefined->ok;
    %     Else2->
    %         net_adm:ping(Else2),
    %         spawn(Else2, player_sup, start_link, [])
    % end,
    % wait(5000),
    % case ?PLAYER3_NODE of
    %     undefined->ok;
    %     Else3->
    %         net_adm:ping(Else3),
    %         spawn(Else3, player_sup, start_link, [])
    % end,
    % wait(5000),
    % case ?PLAYER4_NODE of
    %     undefined->ok;
    %     Else4->
    %         net_adm:ping(Else4),
    %         spawn(Else4, player_sup, start_link, [])
    % end,
    % wait(5000),


    % lists:foreach(fun({_ID, Node})->
    %         io:format("Available name is ~w~n.", [isAvailableName([player1, player2, player3, player4])]),
    %         io:format("Get Active Players: ~w~n", [get_active_players()]),
    %         io:format("got ~w from ~w~n", [net_adm:ping(Node),Node]),
    %         io:format("got ~w from spawn of ~w~n", [spawn(Node, player_sup, start_link, []), Node]),
    %         wait(1000)
    %          end, [{D,N}||{D,N}<-?PLAYER_LIST, N=/=undefined]),
    % io:format("Get Active Players: ~w~n", [get_active_players()]),
    % wait(100000),
    % DataTables = getElemData(),
    % setAllETS(DataTables),
    % %--------------------------------
    % % Start GUI
    % gameGUI:start_monitor(),

    %---------------------------------
    erlang:send_after(1000, self(), keep_init),
    {ok, {initial, 4}}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop()->
    gen_server:cast(?MODULE, stop).

handle_call({update_ball, Ball_Info}, _From, {wait, Stats})->
    ets:insert(ballETS, Ball_Info),
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State)->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(keep_init, {initial, 4}) ->
    io:format("got into node {initial4}, registered: ~w~n", [global:registered_names()]),
    case ?PLAYER1_NODE of
        undefined->ok;
        Else->
            net_adm:ping(Else),
            rpc:block_call(Else, player_sup, start_link, [])
    end,
    wait(500),
    erlang:send_after(1000, self(), keep_init),
    {noreply, {initial, 3}};
handle_info(keep_init, {initial, 3}) ->
    io:format("got into node {initial3}, registered: ~w~n", [global:registered_names()]),
    case ?PLAYER2_NODE of
        undefined->ok;
        Else->
            net_adm:ping(Else),
            rpc:block_call(Else, player_sup, start_link, [])
    end,
    wait(500),
    erlang:send_after(1000, self(), keep_init),
    {noreply, {initial, 2}};
handle_info(keep_init, {initial, 2}) ->
    io:format("got into node {initial2}, registered: ~w~n", [global:registered_names()]),
    case ?PLAYER3_NODE of
        undefined->ok;
        Else->
            net_adm:ping(Else),
            rpc:block_call(Else, player_sup, start_link, [])
    end,
    wait(500),
    erlang:send_after(1000, self(), keep_init),
    {noreply, {initial, 1}};
handle_info(keep_init, {initial, 1}) ->
    io:format("got into node {initial1}, registered: ~w~n", [global:registered_names()]),
    case ?PLAYER4_NODE of
        undefined->ok;
        Else->
            net_adm:ping(Else),
            rpc:block_call(Else, player_sup, start_link, [])
    end,
    wait(500),
    erlang:send_after(1000, self(), keep_init),
    {noreply, {initial, 0}};
handle_info(keep_init, {initial, 0}) ->
    io:format("got into node {initial0}, registered: ~w~n", [global:registered_names()]),
    DataTables = getElemData(),
    setAllETS(DataTables),
    %--------------------------------
    % Start GUI
    gameGUI:start_monitor(),
    ets:setopts(ballETS,{heir, global:whereis_name(gameGUI),[]}),
    ets:setopts(platesETS,{heir, global:whereis_name(gameGUI),[]}),
    ets:setopts(blocksETS,{heir, global:whereis_name(gameGUI),[]}),
    Counter = 0,
    erlang:send_after(1000, self(), set_ball),
    {noreply, {set_ball, Counter}};
handle_info(set_ball, {set_ball, _Counter}) ->
    %--------------------------------
    Balls = ets:tab2list(ballETS),
    lists:foreach(fun({_BALL, ID, Xlocation, Ylocation, XSpeed, YSpeed, BallDiameter, _Player, _BHIT, _POPED, _PlayerS, _playerHit, _WallHit})-> % foreach Ball
        ActiveBalls= get_active_balls(),
        NumBalls = length(ActiveBalls),
        PlayerBoardSize= trunc(?BOARD_WIDTH/NumBalls),

        Ball_IND = lists:zip(lists:seq(1, length(ActiveBalls)), ActiveBalls), % [{Ball_idx, Ball_Name}]

        Balls_relevant =lists:map(fun({Ball_idx, Ball_name})->
                                {Ball_idx, Ball_name, (PlayerBoardSize*(Ball_idx-1)), (PlayerBoardSize*Ball_idx)} end,
                                Ball_IND),

        lists:foreach(fun({_Ball_idx, Ball_name, Left_Border, Right_Border})->
                        case in_range_length(Xlocation, Left_Border, Right_Border) of
                            true->
                                gen_server:cast(Ball_name, {ball_info, #ball{ballID=ID, xlocation=Xlocation, ylocation=Ylocation, xSpeed=XSpeed,
                                    ySpeed=YSpeed, ballRadius=BallDiameter, player=Ball_name}});
                            false->
                                gen_server:call(Ball_name, {no_ball, no_info})
                        end
                    end, Balls_relevant)
                 end, Balls),
    {noreply, {wait, {stats, 0}}}.


%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% get Settings function
% Examples: 
%           1. [GeneralSet, BoardSet, PlateSet, BallSet, BlockSet]= getSettings().
%           2. Settings = getSettings
%------------------------------------------------------------------------------
% getSettings()->
%     % get all element settings by default
%     GeneralSet = #generalSet{},
%     BoardSet = #board{},
%     PlateSet = #plate{},
%     BallSet = #ball{},
%     BlockSet = #block{},
%     [GeneralSet, BoardSet, PlateSet, BallSet, BlockSet].
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
% get_active_players check which players are activated - return list
get_active_balls()->
    RegisteredProc = global:registered_names(),
    [X||X<-RegisteredProc, lists:member(X, [ball1, ball2, ball3, ball4])].
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
% % plateHeight(#board.x_size)) -> 
% plateHeight(PlateWidth)->
%     trunc(PlateWidth/10).
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
    SpeedFactor = element(#generalSet.ballPlateSpeedFactor, #generalSet{}), % get the difault value for speed Factor
    Speed = PlateSpeed*SpeedFactor,
    RandAngle = rand:uniform()*0.5+0.25,
    XSpeed = Speed*RandAngle,
    YSpeed = math:sqrt(1-math:pow(RandAngle, 2))*Speed,
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
wait(Sec) -> 
   receive
   after (Sec) -> ok
end.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% return true if  Left_Boarder < Xlocation < Right_Boarder
in_range_length(Xlocation, Left_Border, Right_Border)->
    case Xlocation > Left_Border of
        true->
            case Xlocation < Right_Border of
                true -> true;
                false -> false
            end;
        false-> false
    end.


%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% isAvailableName([])->
%     no_name_availabel;
% isAvailableName([Elem|T])->
%     case global:whereis_name(Elem) of
%         undefined ->
%             Elem;
%         _Else->
%             isAvailableName(T)        
%     end.




