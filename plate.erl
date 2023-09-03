-module(plate).
-author("Ohad Elgazi").
-behaviour(gen_server).
-include_lib("wx/include/wx.hrl").

%% 
-export([start_link/1, start/1]).
% state functions
-export([]).
%% gen_statem callbacks
-export([init/1, handle_info/2, terminate/3, callback_mode/0, code_change/4, handle_call/3, handle_cast/2]).

-define(BOARD_WIDTH, 1200+?BALL_DIAMETER).
-define(BOARD_HEIGHT, 600).
-record(plate, {plateID, xlocation, ylocation, stepSize, plateWidth, plateHeight, rightGG, leftGG, player}).

start_link([Plate_Num])->
    gen_server:start_link({global, list_to_atom(lists:concat([plate, Plate_Num]))},
     ?MODULE, [Plate_Num], []).
start([Plate_Num])->
    gen_server:start({global, list_to_atom(lists:concat([plate, Plate_Num]))},
     ?MODULE, [Plate_Num], []).

%% API functions
init([_Plate_Num]) ->
    %------------------------------------------------------------------------------------------------------------------------
    % Plate GUI
    wx:new(),
    WX_Frame_obj = wxFrame:new(wx:null(), 1, "Plate Controler"),
    Frame = wx_object:set_pid(WX_Frame_obj, self()), 
    wx_object:set_pid(Frame, self()),
    % build and layout the GUI components
    ButtonLeft = wxButton:new(Frame, 1111, [{label, "Left"}, {size, {200,200}}]),
    ButtonRight = wxButton:new(Frame, 2222, [{label, "Right"}, {size, {200,200}}, {style, 5}]),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, ButtonLeft, [{flag, ?wxEXPAND bor ?wxALL}, {border,5}]),
    wxSizer:add(MainSizer, ButtonRight, [{flag, ?wxEXPAND bor ?wxALL}, {border,5}]),
    wxWindow:setSizer(Frame, MainSizer),
    wxSizer:setSizeHints(MainSizer, Frame),
    wxWindow:setMinSize(Frame, wxWindow:getSize(Frame)),
    wxButton:connect(ButtonLeft, command_button_clicked),
    wxButton:connect(ButtonRight, command_button_clicked),
    wxFrame:show(Frame),
    %------------------------------------------------------------------------------------------------------------------------
    %% Set the initial state + data.  Data is used only as a counter.
    State = wait_plate_info,
    % erlang:send_after(3000, self(), askforsettings),
    Data = {},
    {ok, {State, Data}}.
terminate(_Reason, _State, _Data) ->
    void.
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
callback_mode() -> state_functions.
%-------------------------------------------------------------------------------------------------------------------
% state functions
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
%-------------------------------------------------------------------------------------------------------------------
handle_info({plate_info, set, Plate_info}, {wait_plate_info, _Data})->
    NewData = Plate_info,
    erlang:send_after(1000, self(), check_ball),
    {noreply, {step, NewData}};
handle_info(askforsettings, {wait_plate_info, _Data})->
    gen_server:call({global ,main}, plateInfoPlease),
    {noreply, {wait_plate_info, _Data}};
handle_info({wx,1111, _WxElem, _L, _wxCommand}, {step,Data})->
    Xlocation = element(#plate.xlocation, Data),
    StepSize = element(#plate.stepSize, Data),
    LeftGG = element(#plate.leftGG, Data), 
    LeftLocation = Xlocation-StepSize, 
    if  
        LeftLocation>=LeftGG->
            NewData = Data#plate{xlocation=LeftLocation},
            gen_server:cast({global, main}, {update_elem, plate, NewData}),
            {noreply, {step, NewData}};
        true->
            {noreply, {step, Data}}
    end;
handle_info({wx, 2222, _WxElem, _L, _wxCommand}, {step, Data})->
    Xlocation = element(#plate.xlocation, Data),
    StepSize = element(#plate.stepSize, Data),
    RightGG = element(#plate.rightGG, Data), 
    RightLocation = Xlocation+StepSize, 
    if  
        RightLocation=<RightGG->
            NewData = Data#plate{xlocation=RightLocation},
            gen_server:cast({global, main}, {update_elem, plate, NewData}),
            {noreply, {step, NewData}};
        true->
            {noreply, {step, Data}}
    end;
handle_info(check_ball, {step, Data})->
    Result = gen_server:receive_response(gen_server:send_request({global, main}, ball_info_req), 500),
    case Result of
        timeout-> 
            erlang:send_after(1500, self(), check_ball),
            {noreply, {step, Data}};
        {reply, {ball_info, {Ball, Xlocation, Ylocation, XSpeed, YSpeed, BallRadius}}}->
            Xlocation_plate = element(#plate.xlocation, Data),
            PlateWidth = element(#plate.plateWidth, Data),
            Ylocation_plate = element(#plate.ylocation, Data), 
            PlateHeight = element(#plate.plateHeight, Data),
            case check({Xlocation+XSpeed, Ylocation+YSpeed, BallRadius},
                        {Xlocation_plate, Ylocation_plate, PlateWidth, PlateHeight}) of
                    flipY->
                        gen_statem:cast({global, Ball}, flipYPlate),
                        erlang:send_after(15000, self(), check_ball);
                    _Else->
                        erlang:send_after(1500, self(), check_ball)
            end,                 
            
            {noreply, {step, Data}};
        _Else->
            erlang:send_after(1500, self(), check_ball),
            {noreply, {step, Data}}
    end;
handle_info(_Msg, State)->
    {noreply, State}.
%-----------------------------------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------------------------------
% check if the ball touch the plate
check({Xlocation, Ylocation, BallRadius}, %Check if ball touch plate.
    {Xlocation_plate, Ylocation_plate, PlateWidth, PlateHeight})->
            LowerBall = Ylocation+BallRadius,
            {XLeftBall, XRightBall} = {Xlocation-BallRadius, Xlocation+BallRadius},
            PlateUpBoarder = Ylocation_plate-round(PlateHeight/2),
            {XLeftPlate, XRightPlate} = {trunc(Xlocation_plate-(PlateWidth/2)), trunc(Xlocation_plate+PlateWidth/2)},
            if
                LowerBall>=((?BOARD_HEIGHT)-PlateUpBoarder)+15->
                    if 
                        ((XLeftBall=< XRightPlate) and (XLeftBall >= XLeftPlate)) or 
                    ((XRightBall=< XRightPlate) and (XRightBall >= XLeftPlate))->
                        flipY;
                    true->
                        false
                    end;
                true->
                    false
            end.
%-------------------------------------------------------------------------------------------------------------------
%-------------------------------------------------------------------------------------------------------------------