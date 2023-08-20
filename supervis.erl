-module(supervis).
-author("Ohad Elgazi").

-behaviour(supervisor).

%% API exports
-export([start_link/0]).

%% Behaviour exports
-export([init/1]).


-define(MAIN_NODE, 'main@LAPTOP-A8NM9S74').
-define(PLAYER1_NODE, 'com1@LAPTOP-A8NM9S74').
-define(PLAYER2_NODE, 'com2@LAPTOP-A8NM9S74').
-define(PLAYER3_NODE, undefined).
-define(PLAYER4_NODE, undefined).
-define(PLAYER_LIST, [{player1, ?PLAYER1_NODE}, {player2, ?PLAYER2_NODE}, {player3, ?PLAYER3_NODE}, {player4, ?PLAYER4_NODE}]).




start_link() ->
    %% If needed, we can pass an argument to the init callback function.
    Args = [],
    supervisor:start_link({global, ?MODULE}, ?MODULE, Args).

%% The init callback function is called by the 'supervisor' module.
init([]) ->
    %% Configuration options common to all children.
    %% If a child process crashes, restart only that one (one_for_one).
    %% If there is more than 1 crash ('intensity') in
    %% 5 seconds ('period'), the entire supervisor crashes
    %% with all its children.
    SupFlags = #{strategy => one_for_one,
                 intensity => 2,
                 period => 3},

    %% Specify a child process, including a start function.
    %% Normally the module my_worker would be a gen_server
    %% or a gen_fsm.
                    
    % Children = lists:map(fun({ID, Node})->   % Build All active Nodes
    %             case net_adm:ping(Node) of
    %                     pong->
    %                         case ID of
    %                             main->
    %                                 % supervisor:start_child({global, supervis}, 
    %                                 #{id=>ID,
    %                                     start=>{main, start_link, [{ID2, PlayerNode}||{ID2, PlayerNode}<-?PLAYER_LIST, PlayerNode=/=undefined]}, modules=>[main]}; %Child_Spec
    %                             _Else->
    %                                 % supervisor:start_child({global, supervis},
    %                                 #{id=>ID, 
    %                                     start=>{player, start_link, []}, modules=>[player]} %Child_Spec
    %                         end;
    %                     Else->
    %                         io:format("got other value from pong of ~w, ~w, ~w~n", [Else, ID, Node]),
    %                         ok
    %             end end, [{ID,Node}||{ID, Node}<-?NODES_LIST, Node=/=undefined]),
    
    Children = #{id=>main, start=>{main, start_link, []}},

    % Player1 = #{id => player_1,
    %           start => {player, start_link, [Pla]}},

    %% In this case, there is only one child.
    % Children = [Child],

    %% Return the supervisor flags and the child specifications
    %% to the 'supervisor' module.
    {ok, {SupFlags, Children}}.