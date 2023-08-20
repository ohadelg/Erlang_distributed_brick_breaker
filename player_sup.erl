-module(player_sup).
-author("Ohad Elgazi").
-behaviour(supervisor).

%------------------------------------------------------------------------------
% export functions
%------------------------------------------------------------------------------
% export start function
-export([start_link/0]).
% export Mandatory functions
-export([init/1]).
% export functions
-export([]).


-define(SERVER, ?MODULE).

%------------------------------------------------------------------------------
% import wx library
-define(MAIN_NODE, "main@LAPTOP-A8NM9S74").
-define(PLAYER1, "com1@LAPTOP-A8NM9S74").
-define(PLAYER2, "com2@LAPTOP-A8NM9S74").
-define(PLAYER3, undefined).
-define(PLAYER4, undefined).


%------------------------------------------------------------------------------
% -record(block, {block_id, row, column, xlocation = 50, ylocation = 50, size=60, status=active}).
% -record(plate, {}).
% -record(ball, {}).
% -record(board, {x_size=1920, y_size=960}). % x_size = (numColumns)*blockSpaces, y_size~x_size/2
% -record(generalSet, {numBlocks = 24, numColumns = 12, numRows= 2, blockSpaces = 160}).
%------------------------------------------------------------------------------


start_link()->
    PlayerName = isAvailableName([player1, player2, player3, player4]),
    supervisor:start_link({global, PlayerName}, ?MODULE, [PlayerName]).


init([PlayerName]) ->
    PlayerNum = getElemIndex(PlayerName, [player1, player2, player3, player4]),
    SupFlags = #{strategy => one_for_all,
		 intensity =>10,
		 period => 1,
        auto_shutdown => never},
    ChildSpecs = [
	#{id => ball,
	  start => {ball, start_link, [[PlayerName, PlayerNum]]}}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%    Functions
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------

%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% is available name run on all Elements on list and check if they are available until find one and return it.
isAvailableName([])->
    no_name_availabel;
isAvailableName([Elem|T])->
    case global:whereis_name(Elem) of
        undefined ->
            Elem;
        _Else->
            isAvailableName(T)        
    end.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
getElemIndex(Elem, List)->
    Map = maps:from_list(lists:zip(List, lists:seq(1, length(List)))),
    maps:get(Elem, Map).
