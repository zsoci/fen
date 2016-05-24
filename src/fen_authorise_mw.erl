%% @author zsoci
%% @doc @todo Add description to opr_auth_middleware.

-module(fen_authorise_mw).

-behaviour(cowboy_middleware).

-include("fen_common.hrl").

-export([execute/2]).

-define(FEN_API_PATH_REGEXP_FOR_PUBLIC,
        "(/)|(/ping)|(/v(1)/pub)").
-define(FEN_API_PATH_REGEXP_FOR_AUTH,
        "/v(1)/auth").

%% ====================================================================
%% API functions
%% ====================================================================

%% execute/2
%% ====================================================================
%% @doc Check whether the request has right to proceed
%% @end
-spec execute(Req, Env) -> Result
          when Req :: cowboy_req:req(),
              Env :: term(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
execute(Req, Env) ->
    {Cookies, Req2} = retrieve_cookies(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    case re:run(Path, ?FEN_API_PATH_REGEXP_FOR_PUBLIC) of
        nomatch ->
            check_if_auth_ok(Cookies, Req3, Env);
        {match, _A} ->
            % we have a request that do not need authentication so go ahead
            {ok, Req3, Env}
    end.

check_if_auth_ok(Cookies, Req, Env) ->
    case (Cookies#cookies.user_id =:= undefined) or
         (Cookies#cookies.access_token =:= undefined) or
         (Cookies#cookies.user_token =:= undefined) or
         (Cookies#cookies.device_id =:= undefined) of
        true ->
            maybe_authenticate_request(Cookies, Req, Env);
        false ->
            % here we have all cookies here
            check_user_validity(Cookies, Req, Env)
    end.

%% check_user_validity/3
%% ====================================================================
%% @doc Check if there is a registered user with this token
%% @end
-spec check_user_validity(Cookies, Req, Env) -> Result
          when Cookies :: #cookies{},
              Req :: cowboy_req:req(),
              Env :: term(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
check_user_validity(Cookies, Req, Env) ->
    case fen_a_session:call_local(Cookies#cookies.user_id,
                                  Cookies#cookies.device_id,
                                  {check_access_token, Cookies#cookies.access_token}) of
        {true, _} ->
            {ok, set_meta_tags(Cookies, Req), Env};
        {error, notalive} ->
            {halt, cowboy_req:reply(401, Req)}
    end.

%% maybe_authenticate_request/2
%% ====================================================================
%% @doc Check whether the request is login or register
%% @end
-spec maybe_authenticate_request(Cookies, Req, Env) -> Result
          when 
              Cookies :: term(),
              Req :: cowboy_req:req(),
              Env :: term(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
maybe_authenticate_request(Cookies, Req, Env) ->
    {Path, Req2} = cowboy_req:path(Req),
    case re:run(Path, ?FEN_API_PATH_REGEXP_FOR_AUTH) of
        nomatch ->
            {halt, cowboy_req:reply(401, Req2)};
        {match, _} ->
            % we have an authentication request so go ahead
            lager:debug("Match", []),
            {ok, cowboy_req:set_meta(cookies, Cookies, Req2), Env}
    end.

retrieve_cookies(Req) ->
    {UserToken, Req2} = cowboy_req:cookie(?X_USER_TOKEN, Req),
    {AccessToken, Req3} = cowboy_req:cookie(?X_ACCESS_TOKEN, Req2),
    {DeviceId, Req4} = cowboy_req:cookie(?X_DEVICE_ID, Req3),
    {Host, Req5} = cowboy_req:cookie(?X_HOST, Req4),
    {UserId, Req6} = cowboy_req:cookie(?X_USER_ID, Req5),
    Req7 = cowboy_req:set_meta(user_id, UserToken, Req6),
    Req8 = cowboy_req:set_meta(device_id, DeviceId, Req7),
    {{cookies, UserToken, UserId, AccessToken, DeviceId, Host}, Req8}.

set_meta_tags(Cookies, Req) ->
    Req2 = cowboy_req:set_meta(user_token, Cookies#cookies.user_token, Req),
    Req3 = cowboy_req:set_meta(user_id, Cookies#cookies.user_id, Req2),
    Req4 = cowboy_req:set_meta(access_token, Cookies#cookies.access_token, Req3),
    Req5 = cowboy_req:set_meta(device_id, Cookies#cookies.device_id, Req4),
    cowboy_req:set_meta(host, Cookies#cookies.host, Req5).
