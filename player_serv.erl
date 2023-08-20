-module(player_serv).
-author("Ohad Elgazi").
-behaviour(gen_server).
-export([start_link/0, init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, stop/0]).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    %% Here we ignore what OTP asks of us and just do
    %% however we please.
    io:format("Hello, heavy world!~n"),
    ball:start_link([]),
    % io:format("player_serv is on"),
    {ok, {on}}. % shut down the VM without error


terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop()->
    gen_server:cast(?MODULE, stop).
%------------------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.