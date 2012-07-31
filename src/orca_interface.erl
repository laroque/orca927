-module(orca_interface).

-behaviour(gen_server).


%% API
-export([start_link/0,
         make_connection/0,
         make_connection/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

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
%%---------------------------------------------------------------
make_connection() ->
  make_connection("localhost", 4667).

%%---------------------------------------------------------------
%% Function make_connection(IP, Port)
%% Description: connects to the local instance of ORCA
%%---------------------------------------------------------------
make_connection(IP, Port) ->
  gen_server:cast(?SERVER, {make_connection, IP, Port}).

%%===============================================================
%% gen_server callbacks
%%===============================================================
init(_Args) ->
  {ok, []}.

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
handle_cast({make_connection, IP, Port}, State) ->
  {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {packet, 0}]),
  ok;
handle_cast(_Msg, State) ->
  {noreply, State}.

%%---------------------------------------------------------------
%% Function: handle_info
%%---------------------------------------------------------------
handle_info(hi, State) ->
  io:format("hey there~n"),
  {noreply, State};
handle_info(listen, State) ->
  receive {tcp, _Port, Data} -> Data after 500 -> timeout end;
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