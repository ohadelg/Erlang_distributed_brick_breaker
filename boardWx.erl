-module(boardWx).
-author("Ohad Elgazi").
-behaviour(gen_server).
-export([start_link/1, start/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).
-include_lib("wx/include/wx.hrl").
-define(SERVER, ?MODULE).
%---------------------------------------------------------------------------------------------
% -record(generalSet, {numBlocks = 24,
%                     numColumns = 12,
%                     numRows= 2,
%                     blockSpaces = 160,
%                     ballPlateSpeedFactor=3, % BallSpeed/PlateSpeed
%                     boardPlateWidthFactor = 5, % BoardWidth / PlateWidth
%                     ballRadius = 50, 
%                     blocksETSName = blocksETS,
%                     platesETSName = platesETS,
%                     ballETSName = ballETS}).
%---------------------------------------------------------------------------------------------

% -record(state, {counter, button, counting_down, tref}).

% start functions
start_link([WXWidth, WXheight])->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [WXWidth, WXheight], []).
start([WXWidth, WXheight])->
    gen_server:start({local, ?SERVER}, ?MODULE, [WXWidth, WXheight], []).

% Mandatory callback functions
init([WXWidth, WXheight])->
    
    % Start GUI
    wx:new(),
    Frame = wxFrame:new(wx:null(), -1, "Distributed Concurrency Brick Breaker"),
    % Build Gui Componnets
    sketchBorders(Frame, WXWidth, WXheight), 
    sketchBlocks(Frame),
    sketchPlates(Frame), 
    
    wxFrame:show(Frame),
    {ok, wait}.

terminate(_Reason, _State) ->
    wx:destroy(),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
sketchBorders(Frame, FrameX, FrameY)->
    ActivePlayers = getNumPlayer(),
    Delta = FrameX/ActivePlayers,
    lists:foreach(fun(X)->
        wxGLCanvas:new(Frame, [{pos,{(X-5), 0}},{size, {10,FrameY}}, {palette, wxPalette:new(<<255>>, <<255>>, <<255>>)}]) end, lists:seq(1, ActivePlayers, round(Delta))),
    ok.

%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
sketchBlocks(Frame)->
    BlocksList = ets:tab2list(blocksETS),
    lists:foreach(fun(X)->
        {_ ,_, _, _, Xlocation, Ylocation, Size, Status} = X,
        case Status of
            active-> 
                wxGLCanvas:new(Frame, [{pos,{Xlocation-0.5*Size, Ylocation-0.5*Size}},{size, {Size,Size}}, {palette, wxPalette:new(<<127>>, <<0>>, <<0>>)}]);
            _->
                ok
        end
    end, 
    BlocksList),
    ok.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
sketchPlates(Frame)->
    PlatesList = ets:tab2list(platesETS),
    lists:foreach(fun(X)->
        {_, Xlocation, Ylocation, _, PlateWidth, PlateHieght, _, _, _} = X,
        case active of
            active-> 
                wxGLCanvas:new(Frame, [{pos,{Xlocation-(0.5*PlateWidth), Ylocation-(0.5*PlateHieght)}},{size, {PlateWidth, PlateHieght}}, {palette, wxPalette:new(<<127>>, <<0>>, <<0>>)}]);
            _->
                ok
        end
    end, 
    PlatesList),
    ok.

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



%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
% getBlocks()->
%     % Get blocks for simulation 
%     ets:tab2list(element(#generalSet.blocksETSName, #generalSet{})).
% getblocks(Key)->
%     ets:lookup(element(#generalSet.blocksETSName, #generalSet{}), Key).
% getPlates()->
%     % Get blocks for simulation 
%     ets:tab2list(element(#generalSet.platesETSName, #generalSet{})).
% getPlates(Key)->
%     ets:lookup(element(#generalSet.platesETSName, #generalSet{}), Key).
% getBall()->
%     % Get blocks for simulation 
%     ets:tab2list(element(#generalSet.ballETSName, #generalSet{})).
% getBall(Key)->
%     ets:lookup(element(#generalSet.ballETSName, #generalSet{}), Key).

%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

