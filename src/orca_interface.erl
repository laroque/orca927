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
%% Functions: Wrap for ORCA's 927 set<Parameter> methods
%% WARNING
%% MAYBE THIS IS SUCH GOOD ERLANG... THOUGHTS?
%% WARNING
%%---------------------------------------------------------------
setUpperDiscriminator(Value)->
  callOrcaMCASet('setUpperDiscriminator:0', Value).
setLowerDiscriminator(Value)->
  callOrcaMCASet('setLowerDiscriminator:0', Value).
setLtPreset(Value)->
  callOrcaMCASet('setLtPreset:0', Value).
setPresetCtrlReg()->
  setPresetCtrlReg(8).
setPresetCtrlReg(Value)->
  callOrcaMCASet('setPresetCtrlReg:0', Value).
callOrcaMCASet(Method, Value)->
  gen_server:cast(?SERVER, {orcaMCASetMethod, Method, Value}).

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
