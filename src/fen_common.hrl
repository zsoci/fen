-define(FEN_APP_NAME, fen).
-define(FEN_APP_NAME_B, <<"fen">>).

-define(FEN_FIELD_USER_TOKEN, <<"user_token">>).
-define(FEN_FIELD_PASSWORD, <<"password">>).

%%% HTTP COMMANDS
-define(GET, <<"GET">>).
-define(HEAD, <<"HEAD">>).
-define(OPTIONS, <<"OPTIONS">>).
-define(POST, <<"POST">>).
-define(PUT, <<"PUT">>).
-define(PATCH, <<"PATCH">>).
-define(DELETE, <<"DELETE">>).

%%% HTTP HEADER FIELD elements
-define(APPLICATION, <<"application">>).
-define(JSON, <<"json">>).
-define(TEXT, <<"text">>).
-define(HTML, <<"html">>).
-define(PLAIN, <<"plain">>).
-define(CONTENT_TYPE, <<"content-type">>).
-define(APPLICATION_JSON, <<"application/json">>).

%%% Cookies
-define(X_HOST, <<"X_Host">>).
-define(X_ACCESS_TOKEN, <<"X_Access_Token">>).
-define(X_DEVICE_ID, <<"X_Device">>).
-define(X_USER_TOKEN, <<"X_User_Token">>).
-define(X_USER_ID, <<"X_User_Id">>).

-type options() :: #{ path => string(),
                      model => module(),
                      verbose => boolean()
                    }.

-type state() :: #{ opts => options()
                  }.

-export_type([state/0, options/0]).

-record(cookies,{ user_token = undefined ::binary(),
                  access_token = undefined :: binary(),
                  device_id = undefined :: binary(),
                  host = undefined :: binary(),
                  user_id = undefined :: binary()
                }).

