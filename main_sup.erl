-module(main_sup).
-author("Ohad Elgazi").
-behaviour(supervisor).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
% export functions
%------------------------------------------------------------------------------
% export start function
-export([start_link/0]).
% export Mandatory functions
-export([init/1]).
% export functions
-export([]).
%------------------------------------------------------------------------------
-define(SERVER, ?MODULE).
%------------------------------------------------------------------------------
% import wx library
-define(MAIN_NODE, 'main@LAPTOP-A8NM9S74').
-define(PLAYER1, 'com1@LAPTOP-A8NM9S74').
-define(PLAYER2, 'com2@LAPTOP-A8NM9S74').
-define(PLAYER3, undefined).
-define(PLAYER4, undefined).

-define(PLAYER_NUM, #{player1 => 1, player2 => 2, player3 => 3, player4 => 4}).


%------------------------------------------------------------------------------
% -record(block, {block_id, row, column, xlocation = 50, ylocation = 50, size=60, status=active}).
% -record(plate, {}).
% -record(ball, {}).
% -record(board, {x_size=1920, y_size=960}). % x_size = (numColumns)*blockSpaces, y_size~x_size/2
% -record(generalSet, {numBlocks = 24, numColumns = 12, numRows= 2, blockSpaces = 160}).
%------------------------------------------------------------------------------


start_link()->
    supervisor:start_link({local, mainSuper}, ?MODULE, []).


init([]) ->
    SupFlags = 
        #{strategy => one_for_one,
		 intensity =>10000,
		 period => 1,
         restart => permanent},
    ChildSpecs = [
	% #{id => mainChild,
	%     start => {main, start, [[]]},
    %     restart => permanent,
    %     type=>worker}, 
    #{id => gameGuiChild,
        start => {gameGUI, start_link, []},
        restart => permanent,
        type=>worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%    Functions
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
%.
%.
%.
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
