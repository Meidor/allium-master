-module(auth_service).

%% API
-export([
    client_register/2
]).

client_register(Username, Password) ->
    undefined = persistence_service:select_client(Username),
    SecretHash = base64:encode_to_string(crypto:strong_rand_bytes(50)),
    persistence_service:insert_client(Username, SecretHash, undefined, Password).

