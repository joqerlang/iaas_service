%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(nodes_test_2).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").




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
-define(TEST_NODES,[{"master_sthlm_1",master_sthlm_1@asus},
		    {"dmz_sthlm_1",dmz_sthlm_1@asus},
		    {"worker_sthlm_1",worker_sthlm_1@asus},
		    {"worker_sthlm_2",worker_sthlm_2@asus},
		    {"worker_sthlm_3",worker_sthlm_3@asus}]).
%% 
%% ----------------------------------------------- ---------------------
%% Function:emulate loader
%% Description: requires pod+container module
%% Returns: non
%% --------------------------------------------------------------------
start()->
    [stop_node(Node)||{_NodeId,Node}<-?TEST_NODES],
    [start_node(NodeId,Node)||{NodeId,Node}<-?TEST_NODES],
    ok=application:start(iaas_service),

    ?debugMsg("get_config"),
    ?assertEqual(ok,get_config()),

    ?debugMsg("available"),
    ?assertEqual(ok,available()),
    ?debugMsg("missing"),
    ?assertEqual(ok,missing()),

    ?debugMsg("obsolite"),
    ?assertEqual(ok,obsolite()),
    [stop_node(Node)||{_NodeId,Node}<-?TEST_NODES],
    ok=application:stop(iaas_service),
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
get_config()->
    ?assertEqual([{"master_sthlm_1",master_sthlm_1@asus},
		  {"dmz_sthlm_1",dmz_sthlm_1@asus},
		  {"worker_sthlm_1",worker_sthlm_1@asus},
		  {"worker_sthlm_2",worker_sthlm_2@asus},
		  {"worker_sthlm_3",worker_sthlm_3@asus}],iaas_service:get_config()),
    ok.

available()->
    ?assertEqual([{"iaas_dir_test",iaas_dir_test@asus},
                      {"master_sthlm_1",master_sthlm_1@asus},
                      {"dmz_sthlm_1",dmz_sthlm_1@asus},
                      {"worker_sthlm_1",worker_sthlm_1@asus},
                      {"worker_sthlm_2",worker_sthlm_2@asus},
                      {"worker_sthlm_3",worker_sthlm_3@asus}],iaas_service:available()),
    ok.

missing()->
    stop_node(worker_sthlm_1@asus),
    ?assertEqual([{"worker_sthlm_1",worker_sthlm_1@asus}],iaas_service:missing()),
    start_node("worker_sthlm_1",worker_sthlm_1@asus),
    ?assertEqual([],iaas_service:missing()),
    ok.

obsolite()->
    ?assertEqual([{"iaas_dir_test",iaas_dir_test@asus}],iaas_service:obsolite()),
    ok.
