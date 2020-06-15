%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%%  
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(iaas_service).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Key Data structures
%% 
%% --------------------------------------------------------------------
-record(state,{config}).


%% --------------------------------------------------------------------
%% Definitions 
%% --------------------------------------------------------------------
-define(IAAS_HEARTBEAT,20*1000).
-define(CONFIG_URL,"https://github.com/joqerlang/node_config.git/").
-define(CONFIG_DIR,"node_config").
-define(CONFIG_FILENAME,"node.config").

-export([available/0,missing/0,obsolite/0,get_config/0,update_config/0
	]).

-export([start/0,
	 stop/0,
	 ping/0,
	 heart_beat/1
	]).

%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================

%% Asynchrounus Signals



%% Gen server functions

start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


ping()-> 
    gen_server:call(?MODULE, {ping},infinity).

%%-----------------------------------------------------------------------

%%
available()->
    gen_server:call(?MODULE, {available},infinity).
missing()->
    gen_server:call(?MODULE, {missing},infinity).
obsolite()->
    gen_server:call(?MODULE, {obsolite},infinity).
get_config()->
    gen_server:call(?MODULE, {get_config},infinity).
update_config()->
     gen_server:call(?MODULE, {update_config},infinity).

heart_beat(Interval)->
    gen_server:cast(?MODULE, {heart_beat,Interval}).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%
%% --------------------------------------------------------------------
init([]) ->
    {ok,Config}=nodes:update_config(?CONFIG_URL,?CONFIG_DIR,?CONFIG_FILENAME),
    [net_kernel:connect_node(Node)||{_NodeId,Node}<-Config],  
    spawn(fun()->h_beat(?IAAS_HEARTBEAT) end), 
    {ok, #state{config=Config}}.   
    
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (aterminate/2 is called)
%% --------------------------------------------------------------------
handle_call({ping},_From,State) ->
    Reply={pong,node(),?MODULE},
    {reply, Reply, State};

handle_call({get_config}, _From, State) ->
    Reply=State#state.config,
    {reply, Reply,State};

handle_call({available}, _From, State) ->
    Reply=nodes:available(),
    {reply, Reply,State};
handle_call({missing}, _From, State) ->
    Reply=nodes:missing(State#state.config),
    {reply, Reply,State};
handle_call({obsolite}, _From, State) ->
    Reply=nodes:obsolite(State#state.config),
    {reply, Reply,State};

handle_call({update_config}, _From, State) ->
    Reply=case nodes:update_config(?CONFIG_URL,?CONFIG_DIR,?CONFIG_FILENAME) of
	      {ok,Config}->
		  NewState=State#state{config=Config},
		  ok;
	      {error,Err}->
		  NewState=State,
		  {error,Err}
	  end,
    {reply, Reply,NewState};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% -------------------------------------------------------------------
handle_cast({heart_beat,Interval}, State) ->
    %% Ping all nodes 
    [net_kernel:connect_node(Node)||{_NodeId,Node}<-State#state.config],
    spawn(fun()->h_beat(Interval) end),    
    {noreply, State};

handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{?MODULE,?LINE,Msg}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info(Info, State) ->
    io:format("unmatched match info ~p~n",[{?MODULE,?LINE,Info}]),
    {noreply, State}.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
h_beat(Interval)->
    timer:sleep(Interval),
    rpc:cast(node(),?MODULE,heart_beat,[Interval]).

%% --------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: 
%% Description:
%% Returns: non
%% --------------------------------------------------------------------
