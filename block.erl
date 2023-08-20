-module(block).
-behaviour(gen_statem).

%% 
-export([start_link/1, start/1, callback_mode/0]).

%% gen_statem callbacks
-export([init/1, handle_event/4, terminate/3, destroy/1, step/1]).

%% API functions

% starts the gen_statem process and initializes the state with wait.
start_link([Block_id, Xlocation, Ylocation]) ->
    Name = list_to_atom(lists:concat([block, Block_id])),
    gen_statem:start_link({local, Name}, ?MODULE, [Block_id, Xlocation, Ylocation], []).
start([Block_id, Xlocation, Ylocation])->
    Name = list_to_atom(lists:concat([block, Block_id])),
    gen_statem:start({local, Name}, ?MODULE, [Block_id, Xlocation, Ylocation], []).

% Events Functions
destroy(Block_Name)->
    gen_statem:call(Block_Name, destroy).
step(Block_Name)->
    gen_statem:call(Block_Name, step).

%% Mandatory callback functions
init([Block_id, Xlocation, Ylocation]) ->
    Data = #{block_id=>Block_id, xlocation => Xlocation, ylocation => Ylocation, column=>undefined, row=>undefined},
    {ok, wait , Data}.

terminate(_Reason, _StateData, _StateName) ->
    ok.

% callback decleration
callback_mode() -> handle_event_function. %  state_functions

%% Handle events common to all states
handle_event({call, _From}, destroy, _State, Data) ->
    %% Hitted -> Destroyed
    block_statem:stop("destroyed", Data, wait);
handle_event({call, From}, step, _State, Data)->
    %% for later changes - get data location / store values
    {keep_state,Data,[{reply,From,Data}]};
handle_event({call,From}, get_count, _State, Data) ->
    %% Reply with the current count
    {keep_state,Data,[{reply,From,Data}]};
handle_event(_, _, _State,  Data) ->
    %% Ignore all other events
    {keep_state,Data}.

