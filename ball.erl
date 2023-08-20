-module(ball).
-author("Ohad Elgazi").
-behaviour(gen_statem).

-export([start/1, start_link/1, start_monitor/1,step/0,switch/0,loss/0,stop/0, flipx/0, flipy/0]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([movement/3, wait_ball_info/3]).
-define(BALL_DIAMETER, 30).

-record(ball, {ballID=1, xlocation=1,ylocation=1, xSpeed=0.5, ySpeed=0.5, ballRadius=?BALL_DIAMETER, player, blockHit=0, popedBlocksNum=0, playerSwitch=0, plateHit=0, wallHit=0}).

name() -> ball_statem. % The registered server name

%% API. 
start([Ball_owner, BallID]) ->
    % io:format("setting ball as up"),
    Ball_name = list_to_atom(lists:concat([ball, BallID])),
    gen_statem:start({global, Ball_name}, ?MODULE, [Ball_owner, BallID], []).
start_link([Ball_owner, BallID]) ->
    % io:format("setting ball as up"),
    Ball_name = list_to_atom(lists:concat([ball, BallID])),
    gen_statem:start_link({global,Ball_name}, ?MODULE, [Ball_owner, BallID], []).
start_monitor([Ball_owner, BallID]) ->
    % io:format("setting ball as up"),
    Ball_name = list_to_atom(lists:concat([ball, BallID])),
    gen_statem:start_link({global,Ball_name}, ?MODULE, [Ball_owner, BallID], []).
step()->
    gen_statem:call(name(), step).
switch() ->
    gen_statem:call(name(), switch).
loss() ->
    gen_statem:call(name(), loss).
flipy()->
    gen_statem:call(name(), flipY).
flipx()->
    gen_statem:call(name(), flipX).
stop() ->
    gen_statem:stop(name()).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
init([Ball_owner, BallID]) ->
    %% Set the initial state + data.  Data is used only as a counter.
    State = wait_ball_info, Data = #ball{ballID=BallID ,player=Ball_owner}, 
    % io:format("ball is set up"),
    {ok,State,Data}.
callback_mode() -> state_functions.

%%% state callback(s)
wait_ball_info({call, _From}, {ball_info, Ball_Elem} , _Data)->
    erlang:send_after(1000, self(), step),
    {next_state, movement, {Ball_Elem}}.
movement({call,_From}, loss, _Data) ->
    %% get loss event in cordinates
    gen_statem:stop();
movement({call,_From}, switch, _Data) ->
    %% Switch Player
    gen_statem:stop();
movement({call,From}, flipY, Data) ->
    %% flip y axis movement
    NewData = maps:update(ySpeed, -maps:get(ySpeed, Data), Data),
    {next_state,movement, NewData, [{reply,From, NewData}]};
movement({call,From}, flipX, Data) ->
    %% flip x axis movement
    NewData = maps:update(xSpeed, -maps:get(xSpeed, Data), Data),
    {next_state,movement,NewData,[{reply,From,NewData}]};
movement({call, From}, step, {Ball_Elem})-> % Need to update location to ETS
    %% step forward movement
    {_BALL, _ID, Xlocation, Ylocation, XSpeed, YSpeed, _BallDiameter, _Player, _BHIT, _POPED, _PlayerS, _playerHit, _WallHit} = Ball_Elem,
    NewBall = Ball_Elem#ball{xlocation=(Xlocation+XSpeed), ylocation=(Ylocation+YSpeed)},
    gen_statem:call(main, {update_ball, NewBall}),
    erlang:send_after(1000, self(), step),
    {next_state,movement,NewBall,[{reply,From,NewBall}]};
movement(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).


%% Handle events common to all states
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.

