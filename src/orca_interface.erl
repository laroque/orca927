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
%% Function make_connection()
%% Description: connects to the local instance of ORCA
%%                              on the default port (4667)
%%---------------------------------------------------------------
%%@equiv make_connection("localhost")
make_connection() ->
  make_connection("localhost").

%%---------------------------------------------------------------
%% Function make_connection(IP)
%% Description: connects to the instance of ORCA at IP
%%                              on the default port (4667)
%%---------------------------------------------------------------
%%@equiv make_connection(IP, 4667)
make_connection(IP) ->
  make_connection(IP, 4667).

%%---------------------------------------------------------------
%% Function make_connection(IP, Port)
%% Description: connects to the instance of ORCA at IP
%%                              using on port PORT
%%---------------------------------------------------------------
make_connection(IP, Port) ->
  gen_server:cast(?SERVER, {make_connection, IP, Port}).

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
  {noreply, Newstate};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%---------------------------------------------------------------
%% Function: handle_info
%%---------------------------------------------------------------
handle_info(hi, State) ->
  io:format("hey there~n"),
  {noreply, State};
handle_info(listen, State) ->
  Data = listen(),%receive {tcp, _Port, Data} -> Data after 500 -> timeout end,
  %listen(),
  io:format("orca says:~n"),
  io:format(Data),
  io:format("~n"),
  {noreply, State};
handle_info(_Info, State) ->
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
