%% @author zsoci
%% @doc @todo Add description to opr_auth_middleware.


-module(fen_middleware).

-behaviour(cowboy_middleware).

-include("fen_common.hrl").

-export([execute/2]).

-define(FEN_API_PATH_REGEXP_FOR_PUBLIC,
        "(/)|(/ping)|(/v(1)/pub)").

-record(cookies,{ user_token = undefined ::binary(),
                  access_token = undefined :: binary(),
                  device_id = undefined :: binary(),
                  host = undefined :: binary
                }).

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
            % we have an request that do not need authentication so go ahead
            {ok, Req3, Env}
    end.

check_if_auth_ok(Cookies, Req, Env) ->
    case (Cookies#cookies.access_token =:= undefined) or
         (Cookies#cookies.user_token =:= undefined) of
        true ->
            maybe_authenticate_request(Cookies, Req, Env);
        false ->
            % here we have access token and user token
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
    case um:find_user_id_by_token(Cookies#cookies.user_token) of
        {error, _Reason} ->
            % user does not exists
            maybe_register(Cookies, Req, Env);
        UserId ->
            maybe_session(UserId,
                          cowboy_req:set_meta(user_id,
                                              UserId,
                                              Req),
                          Env)
    end.

maybe_register(_,_,_) ->
    ok.

%% maybe_session/3
%% ====================================================================
%% @doc We have a valid entity id. Check if there is a device id also
%% @end
-spec maybe_session(UserId, Req, Env) -> Result
          when UserId :: binary(),
              Req :: cowboy_req:req(),
              Env :: term(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
maybe_session(UserId, Req, Env) ->
    {DeviceId, Req2} = cowboy_req:cookie(?X_DEVICE_ID, Req),
    case DeviceId of
        DId when (DId =:= undefined) or
                 (DId =:= <<"0">>) or
                 (DId =:= <<>>) ->
            maybe_authenticate_request(a, Req2, Env);
        ValidId ->
            may_have_access_token(UserId, ValidId,
                                  cowboy_req:set_meta(device_id,
                                                      DeviceId,
                                                      Req),
                                  Env)
    end.

%% may_have_access_token/2
%% ====================================================================
%% @doc We have Session and UserID so let us check whether the
%% access token is present
%% cookies are included
%% @end
-spec may_have_access_token(UserId, DeviceId, Req, Env) -> Result
          when Req :: cowboy_req:req(),
              Env :: term(),
              UserId :: string(),
              DeviceId :: string(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
may_have_access_token(UserId, DeviceId, Req, Env) ->
    case cowboy_req:cookie(?X_ACCESS_TOKEN, Req) of
        undefined ->
            maybe_authenticate_request(a, Req, Env);
        {AccessToken, Req2} ->
            try_authenticate(UserId, DeviceId, AccessToken, Req2, Env)
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
    case re:run(Path, ?FEN_API_PATH_REGEXP_FOR_PUBLIC) of
        nomatch ->
            % we shall check whether the request is a v1 here
            case string:substr(binary_to_list(Path), 1, 4) of
                "/v2/" ->
                    {halt,
                     opr_cowboy_lib:reply(401, "",
                                          opr_cowboy_lib:set_unauthenticated(Req2))};
                Else -> % v1 call let us get it through
                    set_v1_call(Req2, Env)
            end;
        {match, A} ->
            % we have an authentication request so go ahead
            {ok, Req2, Env}
    end.

%% try_authenticate/2
%% ====================================================================
%% @doc We have Session and UserID and access token so let us try to
%% authenticate the client
%% @end
-spec try_authenticate(UserId, DeviceId, AccessToken, Req, Env) -> Result
          when Req :: cowboy_req:req(),
              UserId :: string(),
              DeviceId :: string(),
              AccessToken :: string(),
              Env :: term(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
try_authenticate(UserID, DeviceId, AccessToken, Req, Env) ->
    case opr_w_session:call(UserID, DeviceId,
                            {check_access_token, AccessToken}) of
        true ->
            lager:debug("Authenticated. Env=~p", [Env]),
            {ok, Req, Env};
        false ->
            maybe_authenticate_request(a, Req, Env)
    end.

set_v1_call(Req, Env) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {DecodedBody, Req3} = case Body of
                              <<"">> ->
                                  {#{}, Req2};
                              _ ->
                                  {jsx:decode(Body, [return_maps]), Req2}
                          end,
    case opr_utils:retrieve_user_token(DecodedBody) of
        undefined ->
%            {halt,
%             opr_cowboy_lib:reply(400, "",
%                                  opr_cowboy_lib:set_unauthenticated(Req3))};
            {ok, Req3, Env};    
        UserToken ->
            case em:find_entity_id_by_user_token(UserToken) of
                {error, _Reason} ->
                    {halt,
                     opr_cowboy_lib:reply(404, "",
                                          opr_cowboy_lib:set_unauthenticated(Req3))};
                UserId ->
                    Req4 = cowboy_req:set_meta(entity_id, UserId, Req3),
                    Req5 = cowboy_req:set_meta(device_id, <<"0">>, Req4),
                    {ok, Req5, Env}
            end
    end.

printregexp(Path, []) ->
    ok;
printregexp(Path, [{S, L} | Rest]) ->
    if
        S >= 0 ->
            lager:debug("~p~n", [string:substr(binary_to_list(Path), S+1, L)]);
        true ->
            ok
    end,
    printregexp(Path, Rest).

retrieve_cookies(Req) ->
    {UserToken, Req2} = cowboy_req:cookie(?X_USER_TOKEN, Req),
    {AccessToken, Req3} = cowboy_req:cookie(?X_ACCESS_TOKEN, Req2),
    {DeviceId, Req4} = cowboy_req:cookie(?X_DEVICE_ID, Req3),
    {Host, Req5} = cowboy_req:cookie(?X_HOST, Req4),
    Req6 = cowboy_req:set_meta(user_id, UserToken, Req5),
    Req7 = cowboy_req:set_meta(device_id, DeviceId, Req6),
    {{cookies, UserToken, AccessToken, DeviceId, Host}, Req7}.
