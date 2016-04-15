-module(node_service_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([node_register_test/1, node_unregister_test/1]).

all() -> [node_register_test, node_unregister_test].
init_per_testcase(_, Config) ->
    IPaddress = "192.168.4.4",
    Port = 1337,
    PublicKey = "MyPublicKey",
    Node = {IPaddress, Port, PublicKey},
    [{validnode,Node} | Config].

end_per_testcase(_, Config) ->
    Config.

node_register_test(Config) -> 
    {IPaddress, Port, PublicKey} = ?config(validnode, Config)
,    node_service:node_register(IPaddress, Port, PublicKey).

node_unregister_test(Config) ->
    {IPaddress, Port, PublicKey} = ?config(validnode, Config),
    node_service:node_unregister(IPaddress, Port, PublicKey).