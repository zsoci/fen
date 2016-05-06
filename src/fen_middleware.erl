%% @author zsoci
%% @doc @todo Add description to opr_auth_middleware.


-module(fen_middleware).

-behaviour(cowboy_middleware).

-include("opr_common.hrl").
-include("opr_http.hrl").

-export([execute/2]).

-define(OPR_API_PATH_REGEXP_FOR_V2_AUTH,
        "((/)|(/v2/))auth/email/((login$)|(register$))").

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
    {EntityTokenCookie, Req2} = cowboy_req:cookie(?X_SQOR_ENTITY_TOKEN, Req),
    case EntityTokenCookie of
        undefined ->
%            lager:debug("NO entity token",[]),
            maybe_authenticate(Req2, Env);
        EntityToken ->
            check_entity_validity(EntityToken, Req2, Env)
    end.

%% check_entity_validity/3
%% ====================================================================
%% @doc Check if there is a registered user with this token
%% @end
-spec check_entity_validity(EntityToken, Req, Env) -> Result
          when EntityToken :: binary(),
              Req :: cowboy_req:req(),
              Env :: term(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
check_entity_validity(EntityToken, Req, Env) ->
    case em:find_entity_id_by_user_token(EntityToken) of
        {error, _Reason} ->
            maybe_authenticate(Req, Env);
        EntityId ->
            maybe_session(EntityId,
                          cowboy_req:set_meta(entity_id,
                                              EntityId,
                                              Req),
                          Env)
    end.

%% maybe_session/3
%% ====================================================================
%% @doc We have a valid entity id. Check if there is a device id also
%% @end
-spec maybe_session(EntityId, Req, Env) -> Result
          when EntityId :: binary(),
              Req :: cowboy_req:req(),
              Env :: term(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
maybe_session(EntityId, Req, Env) ->
    {DeviceId, Req2} = cowboy_req:cookie(?X_SQOR_DEVICE_ID, Req),
    case DeviceId of
        DId when (DId =:= undefined) or
                 (DId =:= <<"0">>) or
                 (DId =:= <<>>) ->
            maybe_authenticate(Req2, Env);
        ValidId ->
            may_have_access_token(EntityId, ValidId,
                                  cowboy_req:set_meta(device_id,
                                                      DeviceId,
                                                      Req),
                                  Env)
    end.

%% may_have_access_token/2
%% ====================================================================
%% @doc We have Session and EntityID so let us check whether the
%% access token is present
%% cookies are included
%% @end
-spec may_have_access_token(EntityId, DeviceId, Req, Env) -> Result
          when Req :: cowboy_req:req(),
              Env :: term(),
              EntityId :: string(),
              DeviceId :: string(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
may_have_access_token(EntityId, DeviceId, Req, Env) ->
    case cowboy_req:cookie(?X_SQOR_ACCESS_TOKEN, Req) of
        undefined ->
            maybe_authenticate(Req, Env);
        {AccessToken, Req2} ->
            try_authenticate(EntityId, DeviceId, AccessToken, Req2, Env)
    end.

%% maybe_authenticate/2
%% ====================================================================
%% @doc Check whether the request is login or register
%% @end
-spec maybe_authenticate(Req, Env) -> Result
          when Req :: cowboy_req:req(),
              Env :: term(),
              Result :: {ok, Req, Env}
                      | {suspend, Module, Function, Args}
                      | {stop, Req},
              Module :: atom(),
              Function :: atom(),
              Args :: []
                    | list(term()).
maybe_authenticate(Req, Env) ->
    {Path, Req2} = cowboy_req:path(Req),
    case re:run(Path, ?OPR_API_PATH_REGEXP_FOR_V2_AUTH) of
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
%% @doc We have Session and EntityID and access token so let us try to
%% authenticate the client
%% @end
-spec try_authenticate(EntityId, DeviceId, AccessToken, Req, Env) -> Result
          when Req :: cowboy_req:req(),
              EntityId :: string(),
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
try_authenticate(EntityID, DeviceId, AccessToken, Req, Env) ->
    case opr_w_session:call(EntityID, DeviceId,
                            {check_access_token, AccessToken}) of
        true ->
            lager:debug("Authenticated. Env=~p", [Env]),
            {ok, Req, Env};
        false ->
            maybe_authenticate(Req, Env)
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
        EntityToken ->
            case em:find_entity_id_by_user_token(EntityToken) of
                {error, _Reason} ->
                    {halt,
                     opr_cowboy_lib:reply(404, "",
                                          opr_cowboy_lib:set_unauthenticated(Req3))};
                EntityId ->
                    Req4 = cowboy_req:set_meta(entity_id, EntityId, Req3),
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
