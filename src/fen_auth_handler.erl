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

handle_post(Req, State) ->
    A1 =io_lib:format("~p", [lists:flatten(io_lib:format("{\"b\":\"~1000p\"}", [State]))
                            ]),
    {true, cowboy_req:set_resp_body(A1, Req), State}.

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
                              },
                         email =>
                             #{ type => string,
                                description => "Users email (not required)"
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
                             #{ description => "Login Successful"
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
