-module(ball).
-author("Ohad Elgazi").
-behaviour(gen_statem).

-export([start/1, start_link/1, start_monitor/1]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([movement/3, wait_ball_info/3, sleep/3]).
-define(BALL_DIAMETER, 30).
-define(BOARD_WIDTH, 1200+?BALL_DIAMETER).
-define(BOARD_HEIGHT, 600).
-define(TIME_SAC, 50).
-define(TIME_SAC2, 50).

-record(ball, {ballID=1, xlocation=1,ylocation=1, xSpeed=0.5, ySpeed=0.5, ballRadius=?BALL_DIAMETER, player, blockHit=0, popedBlocksNum=0, playerSwitch=0, plateHit=0, wallHit=0}).

%% API. 
start([Ball_owner, BallID]) ->
    Ball_name = list_to_atom(lists:concat([ball, BallID])),
    gen_statem:start({local, Ball_name}, ?MODULE, [Ball_owner, BallID], []).
start_link([Ball_owner, BallID]) ->
    Ball_name = list_to_atom(lists:concat([ball, BallID])),
    gen_statem:start_link({local,Ball_name}, ?MODULE, [Ball_owner, BallID], []).
start_monitor([Ball_owner, BallID]) ->
    Ball_name = list_to_atom(lists:concat([ball, BallID])),
    gen_statem:start_link({local,Ball_name}, ?MODULE, [Ball_owner, BallID], []).


%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
init([Ball_owner, BallID]) ->
    Ball_name = list_to_atom(lists:concat([ball, BallID])),
    global:register_name(Ball_name, self()),
    put(me, Ball_name),
    %% Set the initial state + data.  Data is used only as a counter.
    State = wait_ball_info, Data = #ball{ballID=BallID ,player=Ball_owner}, 
    {ok,State,Data}.
callback_mode() -> state_functions.

%%% state callback(s)
% -----------------------------------------------------------------------------------------------------------------------------------------------
wait_ball_info(info, {ball_info, not_active}, _Data)-> % wait for info and not active ball
    {next_state, sleep, {none}};
% -----------------------------------------------------------------------------------------------------------------------------------------------
wait_ball_info({call, From}, {ball_info, not_active}, _Data)-> % wait for info and not active ball
    {next_state, sleep, {none}, {reply, From, ok}};
% -----------------------------------------------------------------------------------------------------------------------------------------------
wait_ball_info(info, {ball_info, set,  Ball_Elem} , Data)-> % wait for info for start ball movement
    if 
        Ball_Elem==undefined->
            %------------------------------------
            NewData = Data,
            NewState = movement;
            %------------------------------------
        true->
            {_Type, BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball_Elem,
            %------------------------------------
            NewState = movement,
            NewData = #ball{ballID=BallID, xlocation=Xlocation, ylocation=Ylocation, ySpeed=YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
                player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch+1, plateHit=PlateHit, wallHit=WallHit}
            %------------------------------------
    end, 
    erlang:send_after(?TIME_SAC2, self(), step),
    {next_state, NewState, NewData};
% -----------------------------------------------------------------------------------------------------------------------------------------------
wait_ball_info({call, From}, {ball_info, set,  Ball_Elem} , Data)-> % wait for info for start ball movement
    if 
        Ball_Elem==undefined->
            %------------------------------------
            NewData = Data,
            NewState = movement;
            %------------------------------------
        true->
            {_Type, BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball_Elem,
            %------------------------------------
            NewState = movement,
            NewData = #ball{ballID=BallID, xlocation=Xlocation, ylocation=Ylocation, ySpeed=YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
                player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch+1, plateHit=PlateHit, wallHit=WallHit}
            %------------------------------------
    end, 
    erlang:send_after(?TIME_SAC2, self(), step),
    {next_state, NewState, NewData, {reply, From, ok}};
% -----------------------------------------------------------------------------------------------------------------------------------------------
wait_ball_info(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).
% -----------------------------------------------------------------------------------------------------------------------------------------------
% -----------------------------------------------------------------------------------------------------------------------------------------------
sleep({call, From}, {activated, set, Ball_Elem}, _Data)-> % not active - sleep until activate again
    {BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball_Elem,
    NewBall = #ball{ballID=BallID, xlocation=Xlocation, ylocation=Ylocation, ySpeed=YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
         player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch+1, plateHit=PlateHit, wallHit=WallHit},
    {next_step, movement, {NewBall}, [{reply,From, {new_ball_info, NewBall}}]};
% -----------------------------------------------------------------------------------------------------------------------------------------------
sleep(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).
% -----------------------------------------------------------------------------------------------------------------------------------------------
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement(info, step, Ball) ->     %% get loss event in cordinates
    {_Type, BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball,
    case checkBoarders(Xlocation, Ylocation, XSpeed, YSpeed) of
        {NewXSpeed, NewYSpeed}-> % touch Walls
            NewBall = #ball{ballID=BallID, xlocation=Xlocation+NewXSpeed, ylocation=Ylocation+NewYSpeed, ySpeed=NewYSpeed, xSpeed=NewXSpeed, ballRadius=BallRadius,
                player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch, plateHit=PlateHit, wallHit=WallHit},
                gen_server:cast({global, main}, {update_elem, ball, NewBall}),
                erlang:send_after(?TIME_SAC2, self(), step),
                {keep_state, NewBall};
        undefined -> % not Touch Walls
            NewBall = #ball{ballID=BallID, xlocation=Xlocation+XSpeed, ylocation=Ylocation+YSpeed, ySpeed=YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
                player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch, plateHit=PlateHit, wallHit=WallHit},
                gen_server:cast({global, main}, {update_elem, ball, NewBall}),
                erlang:send_after(?TIME_SAC2, self(), step),
                {keep_state, NewBall};
        loss -> %touch the ground
            gen_server:cast({global, gameGUI}, freeze),
            gen_statem:stop(self())
    end;
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement(cast, flipYPlate, Ball) ->     %% flip y axis movement
    {_Type, BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball,
    NewBall = #ball{ballID=BallID, xlocation=Xlocation, ylocation=Ylocation, ySpeed=-YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
         player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch, plateHit=PlateHit+1, wallHit=WallHit},
    {next_state, movement, NewBall};
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement(cast, flipYblock, Ball) ->     %% flip y axis movement
    {_Type, BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball,
    NewBall = #ball{ballID=BallID, xlocation=Xlocation, ylocation=Ylocation, ySpeed=-YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
         player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum+1, playerSwitch=PlayerSwitch, plateHit=PlateHit, wallHit=WallHit},
    {next_state, movement, NewBall};
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement(cast, flipXblock, Ball) ->     %% flip y axis movement
    {_Type, BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball,
    NewBall = #ball{ballID=BallID, xlocation=Xlocation, ylocation=Ylocation, ySpeed=YSpeed, xSpeed=-XSpeed, ballRadius=BallRadius,
         player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum+1, playerSwitch=PlayerSwitch, plateHit=PlateHit, wallHit=WallHit},
    {next_state, movement, NewBall};
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement({call, From}, {switch_on, Ball_Elem} , _Data) ->     %% Switch Player
    {BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball_Elem,
    NewBall = #ball{ballID=BallID, xlocation=Xlocation, ylocation=Ylocation, ySpeed=YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
         player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch+1, plateHit=PlateHit, wallHit=WallHit},
    {next_state, movement, {ball, NewBall}, [reply, From, ok]};
movement({call, From}, switch_off, {ball, _Ball_Elem}) ->     %% Switch Player
    {next_state, sleep, none, [reply, From, ok]};
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement({call,From}, flipY, {ball, Ball_Elem}) ->     %% flip y axis movement
    {BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball_Elem,
    NewBall = #ball{ballID=BallID, xlocation=Xlocation, ylocation=Ylocation+YSpeed, ySpeed=YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
         player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch+1, plateHit=PlateHit, wallHit=WallHit},
    {next_state, movement, {ball, NewBall}, [{reply,From,{new_ball_info, NewBall}}]};
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement({call, From}, flipX, {ball, Ball_Elem}) ->     %% flip x axis movement
    {BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball_Elem,
    NewBall = #ball{ballID=BallID, xlocation=Xlocation+XSpeed, ylocation=Ylocation, ySpeed=YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
         player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch+1, plateHit=PlateHit, wallHit=WallHit},
    {next_state, movement, {ball, NewBall}, [{reply,From,{new_ball_info, NewBall}}]};
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement({call, From}, step, {Ball_Elem})->  %% step forward movement
    {BallID, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius, _Player, BlockHit, PopedBlocksNum, PlayerSwitch, PlateHit, WallHit} = Ball_Elem,
    NewBall = #ball{ballID=BallID, xlocation=Xlocation+XSpeed, ylocation=Ylocation+YSpeed, ySpeed=YSpeed, xSpeed=XSpeed, ballRadius=BallRadius,
         player=get(me), blockHit=BlockHit, popedBlocksNum=PopedBlocksNum, playerSwitch=PlayerSwitch+1, plateHit=PlateHit, wallHit=WallHit},
    gen_statem:cast(main, {update_ball, NewBall}),
    {next_state,movement,{ball, NewBall},[{reply,From,NewBall}]};
% -----------------------------------------------------------------------------------------------------------------------------------------------
movement(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).
% -----------------------------------------------------------------------------------------------------------------------------------------------

%% Handle events common to all states
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.

% -----------------------------------------------------------------------------------------------------------------------------------------------
checkBoarders(Xlocation, Ylocation, XSpeed, YSpeed)->
    if
        Xlocation =< (?BALL_DIAMETER/2)->
            {-XSpeed, YSpeed};
        Xlocation >= (?BOARD_WIDTH-(?BALL_DIAMETER/2))->
            {-XSpeed, YSpeed};
        Ylocation =< (?BALL_DIAMETER/2)->
            {XSpeed, -YSpeed};
        Ylocation >= (?BOARD_HEIGHT-(?BALL_DIAMETER/2))->
            loss;
        true->
            undefined
    end.





