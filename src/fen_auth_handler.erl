%% @author zsoci
%% @doc @todo Add description to fen_healthcheck_handler.


-module(fen_auth_handler).

-behaviour(trails_handler).
-include("fen_common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-include_lib("mixer/include/mixer.hrl").
%-mixin([ fen_default_handler ]).
-mixin([{ fen_default_handler, except, [handle_post/2]}]).

-export([ handle_post/2]).
-export([ trails/0 ]).

-export([login/3,
         register/3]).

handle_post(Req, State) ->
%%     A1 =io_lib:format("~p", [lists:flatten(io_lib:format("{\"b\":\"~1000p\"}", [State]))
%%                             ]),
    lager:debug("~p",[State]),
    #{opts := #{function := Function}} = State,
    {ok, Body, Req2} = cowboy_req:body(Req),
    {DecodedBody, Req3} = case Body of
                              <<"">> ->
                                  {<<"">>, Req2};
                              _ ->
                                  {jsx:decode(Body, [return_maps]), Req2}
                          end,
    ?MODULE:Function(DecodedBody, Req3, State).

-spec trails() -> trails:trails().
trails() ->
    [trails_login(),
     trails_register()].

trails_login() ->
    RequestBody =
        #{ name => <<"request body">>,
           in => body,
           description => <<"request body (as json)">>,
           required => true,
           schema =>
               #{ type => object,
                  required => [user_token, password],
                  properties =>
                      #{ user_token =>
                             #{ type => string,
                                description => "User's login name MD5 hashed"
                              },
                         password =>
                             #{ type => string,
                                format => password,
                                description => "Passowrd MD5 hashed"
                              }
                       }
                }
         },
    Metadata =
        #{ post =>
               #{ tags => ["Authorisation", "Login"],
                  description => "Login",
                  consumes => ["application/json"],
                  produces => ["application/json"],
                  parameters => [RequestBody],
                  responses =>
                      #{ <<"200">> =>
                             #{ description => "Login Successful"
                              },
                         <<"404">> =>
                             #{ description => "Unauthorised"
                              }
                       }
                }
         },
    Path = "/v1/auth/priv/login",
    Opts = #{ path => Path,
              model => zsocimodel,
              verbose => true,
              function => login
            },
    trails:trail(Path, ?MODULE, Opts, Metadata).

trails_register() ->
    RequestBody =
        #{ name => <<"request body">>,
           in => body,
           description => <<"request body (as json)">>,
           required => true,
           schema =>
               #{ type => object,
                  required => [user_token, password, email],
                  properties =>
                      #{ user_token =>
                             #{ type => string,
                                description => "User's login name MD5 hashed"
                              },
                         password =>
                             #{ type => string,
                                format => password,
                                description => "Passowrd MD5 hashed"
                              },
                         email =>
                             #{ type => string,
                                description => "Users email"
                              }
                       }
                }
         },
    Metadata =
        #{ post =>
               #{ tags => ["Authorisation", "Register"],
                  description => "Register",
                  consumes => ["application/json"],
                  produces => ["application/json"],
                  parameters => [RequestBody],
                  responses =>
                      #{ <<"200">> =>
                             #{ description => "Register Successful",
                                schema =>
                                    #{ type => object,
                                       properties =>
                                           #{ message =>
                                                  #{ type => string,
                                                     description => "Register response message"
                                                   }
                                            }
                                     }
                              },
                         <<"403">> =>
                             #{ description => "Existing User"
                              }
                       }
                }
         },
    Path = "/v1/auth/priv/register",
    Opts = #{ path => Path,
              model => zsocimodel,
              verbose => true,
              function => register
            },
    trails:trail(Path, ?MODULE, Opts, Metadata).

login(DecodedBody, Req, State) ->
    case maps:get(?FEN_FIELD_USER_TOKEN, DecodedBody, undefined) of
        undefined ->
            {halt, fen_utils:cowboy_reply(404, "", Req), State};
        Value ->
            %% we have a md5hashed user token here
            %% let us find wether we have a real entity id for it
            maybe_existing_user(Value, DecodedBody, Req, State)
    end.

maybe_existing_user(UserToken, DecodedBody, Req, State) ->
    lager:debug("FINDENTITY:~p.", [UserToken]),
    case um:find_user_id_by_token(UserToken) of
        {error, _Reason} ->
            {halt, fen_utils:cowboy_reply(404, "", Req), State};
        UserId ->
            lager:debug("FOUND:~p.", [UserId]),
            try_login_with_userid(UserId, DecodedBody, Req, State)
    end.

try_login_with_userid(UserId, DecodedBody, Req, State) ->
    % here we have a sqor entity id from the body
    {Cookies, Req2} = cowboy_req:meta(cookies, Req, undefined),
    DeviceId = Cookies#cookies.device_id,
    case Cookies#cookies.user_id =:= UserId of
        true ->
            try_login_user(UserId, DeviceId, DecodedBody,
                           Cookies, Req2, State);
        _Else ->
            %% different user wants to log in
            %% @TODO handle cleaning up the session
            %% for the old user
            try_login_user(UserId, <<"0">>, DecodedBody,
                           Cookies, Req2, State)
    end.

try_login_user(UserId, DeviceId, DecodedBody, Cookies, Req, State) ->
    case fen_a_session:call(UserId,
                            DeviceId,
                            {check_credentials,
                             UserId,
                             maps:get(?FEN_FIELD_PASSWORD, DecodedBody, undefined)}) of
        {ok, NewDeviceId, AccessToken, UserData, Host} ->
            Req2 = fen_utils:update_local_headers(
                     #{?X_DEVICE_ID => integer_to_binary(NewDeviceId),
                       ?X_ACCESS_TOKEN => AccessToken,
                       ?X_USER_TOKEN => maps:get(?FEN_FIELD_USER_TOKEN, DecodedBody, undefined),
                       ?X_HOST => Host,
                       ?X_USER_ID => UserId},
                     Req),
            {true, cowboy_req:set_resp_body(UserData, Req2)};
        {error, Reason} ->
            maybe_login_new_device(Reason, UserId, DeviceId, DecodedBody, Req, Cookies, State)
    end.

maybe_login_new_device(Reason, UserId, DeviceId, DecodedBody,
                       Req, Cookies, State) ->
    case Reason of
        notfound ->
            {halt, fen_utils:cowboy_reply(404, "", Req), State};
        badcredentials ->
            {halt, fen_utils:cowboy_reply(401, "", Req), State};
        missingworker ->
            login_new_device(UserId, DeviceId, Req, State);
        ELSE ->
            {halt, fen_utils:cowboy_reply(500, ELSE, Req)}
    end.

login_new_device(UserId, DeviceId, Req, State) ->
    case DeviceId of
        <<"0">> ->
            WorkerId =
                fen_a_session:create_worker_identity(UserId,
                                                     DeviceId),
            create_missing_worker(WorkerId,
                                  0,
                                  opr_w_session,
                                  Req,
                                  State);
        ELSE ->
            {halt, fen_utils:cowboy_reply(500, ELSE, Req), State}
    end.

create_missing_worker(WorkerIdentity, ClientId, Module,
                      Req, State) ->
    case psh:create_agent(WorkerIdentity, Module,
                          {<<"Base Session">>, ClientId, <<"">>, 0},
                          [{messaging, transient}]) of
        ok ->
            handle_post(Req, State);
        {error, <<"failed">>} ->
            lager:error("Failed to create mssession entity "
                        "for identity:~p", [WorkerIdentity]),
            {halt, fen_utils:cowboy_reply(409, "", Req), State};
        WAFIT ->
            lager:error("Cannot create mssession entity "
                        "for identity:~p~n\nReason:~p",
                        [WorkerIdentity, WAFIT]),
            {halt, fen_utils:cowboy_reply(500, WAFIT, Req), State}
    end.

register(DecodedBody, Req, State) ->
    {true, cowboy_req:set_resp_body(DecodedBody, Req), State}.    
%% 
%% register_entity(UserToken, DecodedBody, Req, _Cookies, State) ->
%%     case em:find_entity_id_by_user_token(UserToken) of
%%         {error, notfound} ->
%%             register_entity_if_pwd(UserToken, DecodedBody, Req, State);
%%         _ ->
%%             {halt, fen_utils:cowboy_reply(403, "Existing User", Req)}
%%     end.
%% 
%% register_entity_if_pwd(UserToken, DecodedBody, Req, State) ->
%%     case maps:get(<<"password">>, DecodedBody) of
%%         undefined ->
%%             {halt, fen_utils:cowboy_reply(403,
%%                                         "Password is missing",
%%                                         Req)};
%%         Password ->
%%             register_entity_with_pwd(UserToken, Password, DecodedBody, Req, State)
%%     end.
%% 
%% register_entity_with_pwd(UserToken, Password, DecodedBody, Req, _State) ->
%%     case em:register_entity(UserToken, Password, DecodedBody) of
%%         {error, exists} ->
%%             {halt, fen_utils:cowboy_reply(401, "Exists", Req)};
%%         {ok, _GUID, Host} ->
%%             lager:error("Host:~p", [Host]),
%%             Req2 = opr_cowboy_lib:update_local_headers(
%%                      #{?X_SQOR_HOST => Host,
%%                        ?X_SQOR_ENTITY_TOKEN => opr_utils:retrieve_user_token(DecodedBody)},
%%                     Req),
%%             {true, cowboy_req:set_resp_body("{}", Req2)};
%%         _ ->
%%             {halt, fen_utils:cowboy_reply(404, "Not implemented", Req)}
%%     end.
