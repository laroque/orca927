-module(orca_interface).

-behaviour(gen_server).

-record(state,{socket,cmd,queue,waiting}).

-record(request,{cmd,sender}).

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
  Command = orca_make_cmd("", "ORMCA927Model", Method, Argslist),
  gen_server:cast(?SERVER, {mcaMethod, Command}).

% Get a parameter =========================================
mca_get(upper_discrim) ->
  mca_get([{getUpperDiscriminator, 0}]);
mca_get(Method) ->
  Command = orca_make_cmd("get", "ORMCA927Model", Method, []),
  gen_server:call(?SERVER, #request{cmd=Command}).

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
  Command = orca_make_cmd("", "ORMCA927Model", Methodlist, Argslist),
  gen_server:cast(?SERVER, {mcaMethod, Command}).

%%===============================================================
%% gen_server callbacks
%%===============================================================
%%---------------------------------------------------------------
%% Function: init
%%---------------------------------------------------------------
init(_Args) ->
  InitialState = #state{
    socket = 0,
    cmd = "",
    queue = [],
    waiting = false
  },
  io:format("initial state:~p~n",[InitialState]),
  {ok, InitialState}.

%%---------------------------------------------------------------
%% Function: handle_call
%%---------------------------------------------------------------
handle_call(#request{cmd=Command}=Request, From,
            #state{socket=Soc, cmd=_CMD, queue=_Q, waiting=false}=State) ->
  NewState = State#state{
    cmd = Request#request{sender=From},
    waiting = true
  },
  gen_tcp:send(Soc, Command),
  erlang:send_after(2000, self(), tcp_timeout),
  {noreply, NewState};
handle_call(#request{}=Request, From, #state{queue=Q, waiting=true}=State) ->
  NewState = State#state{
    queue = Q ++ [Request#request{sender=From}]
  },
  {noreply, NewState};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%---------------------------------------------------------------
%% Function: handle_cast
%%---------------------------------------------------------------
handle_cast({make_connection, IP, Port}, State) ->
  {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}]),
  Newstate = State#state{socket=Socket},
  io:format("~p~n",[Socket]),
  {noreply, Newstate};
handle_cast({mcaMethod, {Command}}, #state{socket=Soc}=State) ->
  gen_tcp:send(Soc, Command),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%---------------------------------------------------------------
%% Function: handle_info
%%---------------------------------------------------------------
handle_info({tcp, _Soc, <<"OrcaHeartBeat\n">>}, State) ->
  {noreply, State};
handle_info(tcp_timeout, #state{cmd=#request{sender=From}, socket=_S, queue=[]}=State) ->
  gen_server:reply(From, {error, tcp_timeout}),
  {noreply, State#state{waiting=false}};
handle_info({tcp, Soc, Data}, #state{cmd=#request{sender=From}, socket=Soc, queue=[]}=State) ->
  gen_server:reply(From, Data),
  {noreply, State#state{waiting=false}};
handle_info({tcp, Soc, Data}, #state{cmd=#request{sender=From}, socket=Soc, queue=[N|T]}=State) ->
  Command=N#request.cmd,
  gen_server:reply(From, Data),
  gen_tcp:send(Soc, Command),
  NewState = State#state{
    cmd = N,
    queue = T
  },
  {noreply, NewState};
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
%----------------------------------------------------------------
% Generate a command string to send to orca via TCP socket
%----------------------------------------------------------------
orca_make_cmd("", Module, Method, Argslist) ->
  {lists:concat(["[", Module, " ",
                 unpack_arg_list(Method), unpack_arg_list(Argslist), "]"])};
orca_make_cmd(Tagstring, Module, Method, Argslist) ->
  {lists:concat([Tagstring, " = [", Module, " ",
                 unpack_arg_list(Method), unpack_arg_list(Argslist), "]"])}.
% the one you call
unpack_arg_list(Arglist) ->
  unpack_arg_list(Arglist, []).
% base cases
unpack_arg_list([{}], Acc) ->
  Acc;
unpack_arg_list([], Acc) ->
  Acc;
% recurse
unpack_arg_list([{Name, Value}|T], Acc) ->
  Str = lists:concat([Name, ":", Value, " "]),
  unpack_arg_list(T, Acc ++ Str).
