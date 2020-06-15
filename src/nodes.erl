%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(nodes).
 


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%-compile(export_all).
-export([available/0,missing/1,obsolite/1,update_config/3]).




%% ====================================================================
%% External functions
%% ====================================================================

%% @doc: update_config(GitUrl,Dir,FileName)->{ok,Config}|{error,Err} retreives the latets  config spec from git

-spec(update_config(GitUrl::string(),Dir::string(),FileName::string())->{ok,Config::[tuple()]}|{error,Err::string()}).
update_config(GitUrl,Dir,FileName)->
    os:cmd("rm -rf "++Dir),
    os:cmd("git clone "++GitUrl),
    {R,Info}=file:consult(filename:join(Dir,FileName)),
    {R,Info}.

%% --------------------------------------------------------------------
%% 
%% 
%% {"master_sthlm_1",'master_sthlm_1@asus'}
%% --------------------------------------------------------------------
%% @doc: available get active nodes in the cluster

-spec(available()->[{NodeId::string(),Node::atom()}]| []).
available()->
    AvailableNodes=[node()|nodes()],
    Node_NodeId_Host=[{Node,string:split(atom_to_list(Node),"@")}||Node<-AvailableNodes],
    NodeId_Node=[{NodeId,Node}||{Node,[NodeId,_Host]}<-Node_NodeId_Host],
    NodeId_Node.


%% @doc: missing nodes in the cluster

-spec(missing(AllNodes::tuple())->[{NodeId::string(),Node::atom()}]| []).
missing(AllNodes)->
    AvailableNodes=available(),
    Missing=[{NodeId,Node}||{NodeId,Node}<-AllNodes,
			    false==lists:member({NodeId,Node},AvailableNodes)],
    Missing.

%% @doc: obsolite nodes in the cluster

-spec(obsolite(AllNodes::tuple())->[{NodeId::string(),Node::atom()}]| []).
obsolite(AllNodes)->
    AvailableNodes=available(),
    Obsolite=[{NodeId,Node}||{NodeId,Node}<-AvailableNodes,
			    false==lists:member({NodeId,Node},AllNodes)],
    Obsolite.
