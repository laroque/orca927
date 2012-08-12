-module(orca_interface).

-behaviour(gen_server).

-record(state,{socket}).

%% API
-export([start_link/0,
         make_connection/0, make_connection/1, make_connection/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%%Just while coding/debugging:
-compile([debug_info, export_all]).

-define(SERVER, ?MODULE).

%%===============================================================
%% API
%%===============================================================

%%---------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%---------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%---------------------------------------------------------------
%% Function make_connection(IP, Port)
%% Description: connects to an instance of orca using the provided
%% IP and port, if no port is given uses the default 4667, if no 
%% IP is given uses localhost
%%---------------------------------------------------------------

%%@equiv make_connection("localhost")
make_connection() ->
  make_connection("localhost").
%%@equiv make_connection(IP, 4667)
make_connection(IP) ->
  make_connection(IP, 4667).
make_connection(IP, Port) ->
  gen_server:cast(?SERVER, {make_connection, IP, Port}).

%%---------------------------------------------------------------
%% Functions: Wraps for ORCA MCA methods
%%---------------------------------------------------------------
% Set a parameter =========================================
% Default values (where reasonable):
mca_set(upper_discrim) ->
  mca_set(upper_discrim, 16383);
mca_set(lower_discrim) ->
  mca_set(lower_discrim, 0);
mca_set(presetMode) ->
  mca_set(presetMode, 8).
% User supplied values:
mca_set(upper_discrim, Value) ->
  mca_set([{setUpperDiscriminator, 0}], [{withValue, Value}]);
mca_set(lower_discrim, Value) ->
  mca_set([{setLowerDiscriminator, 0}], [{withValue, Value}]);
mca_set(ltTimeout, Value) ->
  mca_set([{setLtPreset, 0}], [{withValue, Value}]);
mca_set(presetMode, Value) ->
  mca_set([{setPresetCtrlReg, 0}], [{withValue, Value}]);
mca_set(comment, Value) ->
  mca_set([{setComment, Value}], []);
mca_set(Method, Argslist) ->
  orca_make_cmd("", "ORMCA927Model", Method, Argslist).

% Do a thing ==============================================
mca_do(clearSpectrum) ->
  mca_do([{clearSpectrum, 0}], []);
mca_do(startSpectrum) ->
  mca_do([{startAcquisition, 0}], []);
mca_do(stopSpectrum) ->
  mca_do([{stopAcquisition, 0}], []).
mca_do(saveSpectrum, Filename) ->
  mca_do([{writespectrum, 0}], [{toFile, Filename}]);
mca_do(Methodlist, Argslist) ->
  orca_make_cmd("", "ORMCA927Model", MethodList, Argslist).

%%===============================================================
%% gen_server callbacks
%%===============================================================
init(_Args) ->
  {ok, #state{socket=0}}.

%%---------------------------------------------------------------
%% Function: handle_call
%%---------------------------------------------------------------
listen() ->
  receive {tcp, _Port, Data} -> Data after 500 -> timeout end.

%handle_call({get_orca_response}, _From, State) ->
%  Data = listen(),
%  io:format(Data),
%  Reply = ok,
%  {reply, Reply, State}.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%---------------------------------------------------------------
%% Function: handle_cast
%%---------------------------------------------------------------
handle_cast({make_connection, IP, Port}, _State) ->
  {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}]),
  Newstate = #state{socket=Socket},
  io:format("~p~n",[Socket]),
  {noreply, Newstate};
handle_cast({setUpperDiscriminator, Value}, #state{socket=Soc}=State) ->
  gen_tcp:send(Soc, lists:concat(["[ORMCA927Model setUpperDiscriminator:0 withValue:",Value,"]"])),
  {noreply, State};
handle_cast({orcaMCASetMethod, Method, WithValue}, #state{socket=Soc}=State) ->
  gen_tcp:send(Soc, lists:concat(["[ORMCA927Model ",Method," withValue:",WithValue,"]"])),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%---------------------------------------------------------------
%% Function: handle_info
%%---------------------------------------------------------------
handle_info(hi, State) ->
  io:format("hey there~n"),
  {noreply, State};
handle_info(hardset, #state{socket=Soc}=State) ->
  gen_tcp:send(Soc, "hdset=[ORMCA927Model setUpperDiscriminator:0 withValue:1000"),
  io:format("with state:~p~n",[State]),
  {noreply, State};
handle_info(setULD, #state{socket=Soc}=State) ->
  gen_tcp:send(Soc, orca927_mkcmds:setUpperDiscriminator(16000)),
  {noreply, State};
handle_info({tcp, _Soc, <<"OrcaHeartBeat\n">>}, State) ->
  {noreply, State};
handle_info(_Info, State) ->
  io:format("~n Got a Response:~n~p~nfrom Orca~n",[_Info]),
  {noreply, State}.

%%---------------------------------------------------------------
%% Function: terminate
%%---------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%---------------------------------------------------------------
%% Function: code_change
%%---------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%===============================================================
%% Internal Functions
%%===============================================================
orca_make_cmd("", Module, Method, Argslist) ->
  {lists:concat([Tagstring, " = [", Module, " ",
                 unpack_arg_list(Method), unpack_arg_list(Argslist), "]"])}.
unpack_arg_list(Arglist) ->
  unpack_arg_list([{}]), []);
unpack_arg_list([{}], Acc) ->
  Acc;
unpack_arg_list([], Acc) ->
  Acc;
unpack_arg_list([{Name, Value}|T], Acc) ->
  Str = lists:concat([Name, ":", Value, " "]),
  unpack_arg_list(T, Acc ++ Str).
