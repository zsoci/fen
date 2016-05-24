%% @author zsoci
%% @doc @todo Add description to fen_utils.


-module(fen_utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([atom_to_method/1]).

-spec atom_to_method(get|patch|put|post|delete) -> binary().
atom_to_method(get) -> <<"GET">>;
atom_to_method(patch) -> <<"PATCH">>;
atom_to_method(put) -> <<"PUT">>;
atom_to_method(post) -> <<"POST">>;
atom_to_method(delete) -> <<"DELETE">>.

%% set_unauthenticated(Req) ->
%%     Error1 = cowboy_req:set_resp_header(
%%                <<"Www-Authenticate">>,
%%                <<"Basic realm=\"Secure Area\"">>, Req),
%%     _ErrorReq = cowboy_req:set_resp_body(
%%                   unauthenticated_body(), Error1).

%% ====================================================================
%% Internal functions
%% ====================================================================


