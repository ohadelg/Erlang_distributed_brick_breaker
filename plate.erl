-module(plate).
-author("Ohad Elgazi").
-behaviour(gen_statem).

%% 
-export([start_link/1, start/1, step_left/0, step_right/0, stop/0]).
% state functions
-export([stand/3]).
%% gen_statem callbacks
-export([init/1, handle_event/3, terminate/3, callback_mode/0, code_change/4]).

name()-> plate_statem.

start_link([Plate_ID, Xlocation, Ylocation, RightGG, LeftGG, Plate_size])->
    gen_statem:start_link({local, name()}, ?MODULE, [Plate_ID, Xlocation, Ylocation, RightGG, LeftGG, Plate_size], []).
start([Plate_ID, Xlocation, Ylocation, RightGG, LeftGG, Plate_size])->
    gen_statem:start({local, name()}, ?MODULE, [Plate_ID, Xlocation, Ylocation, RightGG, LeftGG, Plate_size], []).

%% API functions
init([Plate_ID,Xlocation, Ylocation, RightGG, LeftGG, Plate_size]) ->
    %% Set the initial state + data.  Data is used only as a counter.
    State = stand, Data = #{plate_id => Plate_ID, xlocation =>Xlocation,ylocation =>Ylocation, step_size=>0.2,
             rightGG=>RightGG, leftGG=>LeftGG, plate_size => Plate_size}, 
    {ok,State,Data}.
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
callback_mode() -> state_functions.

% Events Functions
step_right()->
    gen_statem:call(name(), step_right).
step_left()->
    gen_statem:call(name(), step_left).
stop()->
    gen_statem:call(name(), stop).

% state functions
stand({call, From}, step_right, Data)->
    X = maps:get(xlocation, Data) + maps:get(plate_size, Data),
    X_gg = maps:get(rightGG, Data),
    if
        X>=X_gg ->
            {next_state, stand, Data, [{reply,From, Data}]};
        true->
            NewData = Data#{xlocation:=(maps:get(xlocation, Data)+maps:get(step_size, Data))},
            {next_state, stand, NewData, [reply, From, NewData]}
    end;
stand({call, From}, step_left, Data)->
    X = maps:get(xlocation, Data) - maps:get(plate_size, Data),
    X_gg = maps:get(rightGG, Data),
    if
        X=<X_gg ->
            {next_state, stand, Data, [{reply,From, Data}]};
        true->
            NewData = Data#{xlocation:=(maps:get(xlocation, Data)-maps:get(step_size, Data))},
            {next_state, stand, NewData, [reply, From, NewData]}
    end;
stand({call, _From}, loss, _Data)->
    gen_statem:stop();
stand(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

%% Handle events common to all states
handle_event({call,From}, get_count, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, Data) ->
    %% Ignore all other events
    {keep_state,Data}.
