%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(nodes_test).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("common_macros.hrl").



-ifdef(dir).
-define(CHECK_CATALOG,check_catalog_dir()).
-else.
-define(CHECK_CATALOG,check_catalog_git()).
-endif.


%% --------------------------------------------------------------------
-export([start/0]).
%-compile(export_all).



%% ====================================================================
%% External functions
%% ====================================================================
-define(TEST_NODES,[{"node_sthlm_1",'node_sthlm_1@asus'},
		    {"worker_varmdoe_1",'worker_varmdoe_1@asus'},
		    {"worker_sthlm_1",'worker_sthlm_1@asus'},
		    {"worker_sthlm_2",'worker_sthlm_2@asus'}]).
%% 
%% ----------------------------------------------- ---------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
start()->
    [stop_node(Node)||{_NodeId,Node}<-?TEST_NODES],
    [start_node(NodeId,Node)||{NodeId,Node}<-?TEST_NODES],

    ?debugMsg("available"),
    ?assertEqual(ok,available()),
    ?debugMsg("missing"),
    ?assertEqual(ok,missing()),

    ?debugMsg("obsolite"),
    ?assertEqual(ok,obsolite()),
    [stop_node(Node)||{_NodeId,Node}<-?TEST_NODES],
    ok.


start_node(NodeId,Node)->
    []=os:cmd("erl -sname "++NodeId++" -detached"),
    check_node_start(Node,20).

check_node_start(Node,T)->
    case net_adm:ping(Node) of
	pong ->
	    ok;
	pang->
	    timer:sleep(T),
	    check_node_start(Node,T)
    end.

stop_node(Node)->
    rpc:call(Node,init,stop,[]),
    check_node_stop(Node,20).

check_node_stop(Node,T)->
    case net_adm:ping(Node) of
	pang ->
	    ok;
	pong->
	    timer:sleep(T),
	    check_node_stop(Node,T)
    end.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
available()->
    ?assertEqual([{"iaas_dir_test",iaas_dir_test@asus},
		 {"node_sthlm_1",node_sthlm_1@asus},
		 {"worker_varmdoe_1",worker_varmdoe_1@asus},
		 {"worker_sthlm_1",worker_sthlm_1@asus},
		 {"worker_sthlm_2",worker_sthlm_2@asus}],nodes:available()),
    ok.

missing()->
    stop_node(node_sthlm_1@asus),
    ?assertEqual([{"node_sthlm_1",node_sthlm_1@asus}],nodes:missing(?TEST_NODES)),
    start_node("node_sthlm_1",node_sthlm_1@asus),
        ?assertEqual([],nodes:missing(?TEST_NODES)),
    ok.

obsolite()->
    ?assertEqual([{"iaas_dir_test",iaas_dir_test@asus},
		  {"node_sthlm_1",node_sthlm_1@asus}],nodes:obsolite([{"worker_varmdoe_1",worker_varmdoe_1@asus},
				      {"worker_sthlm_1",worker_sthlm_1@asus},
				      {"worker_sthlm_2",worker_sthlm_2@asus}])),
        ?assertEqual([{"iaas_dir_test",iaas_dir_test@asus}],nodes:obsolite(?TEST_NODES)),
    ok.
