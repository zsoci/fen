%% @author zsoci
%% @doc @todo Add description to ctr_test.


-module(fen_test).

-include("fen_common.hrl").
-include_lib("psh/include/psh.hrl").
-define(URL, "http://localhost:8081").
-define(LOCAL_NODE_NAME, begin {ok, HostName}=inet:gethostname(), HostName end).
-define(APPL_JSON, "application/json").
-define(BUCKETTYPES, ["strong", "default"]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([login/0,
         login_v1/0,
         register/0,
         register_v1/0,
%         send/0,
         list/0,
         get_entity/1,
         get_entity_v1/1,
         pget/0,
         pget/1,
         get/1,
         get/0]).

-spec login() -> term().
login() ->
    httpc:set_options([{cookies, enabled}]),
    ContentType = ?APPL_JSON,
    Body = "{\"rememberme\":false, "
        "\"email\":\"zsociii@lamardan.com\", "
%        "\"email\":\"flavio@inaka.net\", "
        "\"password\":\"PASSWORD\"}",

    {ok, {{Version, Code, ReasonPhrase}, Headers, RespBody}} =
        httpc:request(post, {?URL ++ "/v2/auth/email/login",
                             [], ContentType, Body}, [], []),

    io:format("~n~p,~p,~p, Headers:~p, Body:~p~n", [Version,
                                             Code,
                                             ReasonPhrase,
                                             Headers,
                                             RespBody]),
    httpc:store_cookies(Headers, "/"),
    httpc:which_cookies().

-spec login_v1() -> term().
login_v1() ->
    httpc:set_options([{cookies, enabled}]),
    ContentType = ?APPL_JSON,
    Body = "{\"rememberme\":false, "
        "\"email\":\"zsociii@lamardan.com\", "
%        "\"email\":\"flavio@inaka.net\", "
        "\"password\":\"PASSWORD\"}",

    {ok, {{Version, Code, ReasonPhrase}, Headers, RespBody}} =
        httpc:request(post, {?URL ++ "/auth/email/login",
                             [], ContentType, Body}, [], []),

    io:format("~n~p,~p,~p, Headers:~p, Body:~p~n", [Version,
                                             Code,
                                             ReasonPhrase,
                                             Headers,
                                             RespBody]),
    httpc:store_cookies(Headers, "/"),
    httpc:which_cookies().

-spec register() -> term().
register() ->
    httpc:set_options([{cookies, enabled}]),
    ContentType = ?APPL_JSON,
    Body = "{\"rememberme\":false, "
        "\"email\":\"zsociii@lamardan.com\", "
        "\"password\":\"PASSWORD\"}",

    {ok, {{Version, Code, ReasonPhrase}, Headers, RespBody}} =
        httpc:request(post, {?URL ++ "/v2/auth/email/register",
                             [], ContentType, Body}, [], []),

    io:format("~n~p,~p,~p, Headers:~p~nBody:~p~n", [Version,
                                             Code,
                                             ReasonPhrase,
                                             Headers,
                                             RespBody]),
    httpc:store_cookies(Headers, "/"),
    httpc:which_cookies().

-spec register_v1() -> term().
register_v1() ->
    httpc:set_options([{cookies, enabled}]),
    ContentType = ?APPL_JSON,
    Body = "{\"rememberme\":false, "
        "\"email\":\"zsociii@lamardan.com\", "
        "\"password\":\"PASSWORD\"}",

    {ok, {{Version, Code, ReasonPhrase}, Headers, RespBody}} =
        httpc:request(post, {?URL ++ "/auth/email/register",
                             [], ContentType, Body}, [], []),

    io:format("~n~p,~p,~p, Headers:~p~nBody:~p~n", [Version,
                                             Code,
                                             ReasonPhrase,
                                             Headers,
                                             RespBody]),
    httpc:store_cookies(Headers, "/"),
    httpc:which_cookies().

-spec pget() -> term().
pget() ->
    pget(0).

-spec pget(NS :: non_neg_integer()) -> term().
pget(SN) ->
    login(),
    [{session_cookies,Cookies}] = httpc:which_cookies(),
    [Token, _Device] = get_cookies(["X_Sqor_Access_token", "X_Sqor_Device"],
                                  Cookies),
    [_, UUID] = string:tokens(Token, "_"),
    _Pid = proc_lib:spawn(?MODULE, get, [SN]),
    W = fen_a_user:create_agent_identity(list_to_binary(UUID)),
    Msg = #message{message = <<"This is a message">>,
                   type = persistent},
    psh:call(message, W, Msg).

%% get_cookie(Name, Cookies) ->
%%     [Value] = get_cookies([Name], Cookies).

get_cookies(Names, Cookies) ->
    get_cookies(Names, Cookies, []).

get_cookies([], _, Values) -> lists:reverse(Values);
get_cookies([Name | Rest], Cookies, Values) ->
    case lists:keyfind(Name, 4, Cookies) of
        false ->
            get_cookies(Rest, Cookies, Values);
        Tuple ->
            get_cookies(Rest, Cookies, [element(5, Tuple) | Values])
    end.

-spec get() -> term().
get() ->
    ?MODULE:get(0).

-spec get(SN :: non_neg_integer()) -> term().
get(SN) ->
    httpc:set_options([{cookies, enabled}]),
    ContentType = ?APPL_JSON,
    Body = "{\"MsgSerial\":" ++ integer_to_list(SN) ++ "}",
    {ok, {{Version, Code, ReasonPhrase}, Headers, RespBody}} =
        httpc:request(post, {?URL ++ "/v2/get",
                             [], ContentType, Body}, [], []),

    io:format("~n~p,~p,~p, Headers:~p,~nBody:~p~n", [Version,
                                             Code,
                                             ReasonPhrase,
                                             Headers,
                                             RespBody]),
    httpc:store_cookies(Headers, "/"),
    httpc:which_cookies().
    

-spec get_entity(ID :: string()) -> term().
get_entity(Id) ->
    {ok, {{Version, Code, ReasonPhrase}, Headers, RespBody}} =
        httpc:request(?URL ++ "/v2/entities/" ++ Id),
    
    io:format("~n~p,~p,~p, Headers:~p,~nBody:~p~n", [Version,
                                             Code,
                                             ReasonPhrase,
                                             Headers,
                                             RespBody]),
    httpc:store_cookies(Headers, "/"),
    httpc:which_cookies().

-spec get_entity_v1(Id :: string()) -> term().
get_entity_v1(Id) ->
    {ok, {{Version, Code, ReasonPhrase}, Headers, RespBody}} =
        httpc:request(?URL ++ "/entities/" ++ Id),
    
    io:format("~n~p,~p,~p, Headers:~p,~nBody:~p~n", [Version,
                                             Code,
                                             ReasonPhrase,
                                             Headers,
                                             RespBody]),
    httpc:store_cookies(Headers, "/"),
    httpc:which_cookies().

-spec list() -> term().
list() ->
    {ok, Pid} = riakc_pb_socket:start_link(application:get_env(wrk, riak_ip, "127.0.0.1"),
                                           application:get_env(wrk, riak_port, 10017)),
    _Buckets = get_buckets(Pid, ?BUCKETTYPES),
    riakc_pb_socket:stop(Pid)

%   io:format("~s", [Buckets]),
%   {ok, L} = riakc_pb_socket:list_keys(Pid, {<<"strong">>, <<"Actors_msuser">>}),
%   io:format("~s", [L])
.

get_buckets(_Pid, []) -> [];
get_buckets(Pid, [Type|Rest]) ->
    {ok, Buckets} = riakc_pb_socket:list_buckets(Pid, list_to_binary(Type)),
    io:format("==============================\nType:~p\n", [Type]),
    list_buckets(Pid, Type, Buckets),
    get_buckets(Pid, Rest)
.

list_buckets(_Pid, _Type, []) -> [];
list_buckets(Pid, Type, [Bucket|Rest]) ->
    io:format("------------------------\nBucket:~p\n", [Bucket]),
    case riakc_pb_socket:list_keys(Pid, {list_to_binary(Type), Bucket}) of
        {ok, Keys} ->
            list_keys(Pid, {list_to_binary(Type), Bucket}, Keys);
        WAFIT ->
            io:format("Error:~p", [WAFIT])
    end,
    list_buckets(Pid, Type, Rest)
.

list_keys(_Pid, _Bucket, []) -> [];
list_keys(Pid, Bucket, [Key|Rest]) ->
    Term = try binary_to_term(Key) of
               T ->
                   T
           catch
               _:_ ->
                   binary_to_list(Key)
           end,
    io:format("...........................\nKey:~p~n", [Term]),
    case riakc_pb_socket:get(Pid, Bucket, Key) of
        {ok, Obj} ->
            Value = riakc_obj:get_value(Obj),
            V1 = try binary_to_term(Value) of
                     ANY -> ANY
                 catch
                     _:_ ->
                         binary_to_list(Value)
                 end,
            case is_list(V1) of
                true ->io:format("List:~s\n", [V1]);
                _ -> io:format("Term:~p\n", [V1])
            end,

            io:format("Content type:~p\n", [riakc_obj:get_content_types(Obj)]);
        WAFIT ->
            io:format("Error:~p\n", [WAFIT])
    end,
    list_keys(Pid, Bucket, Rest)
.


%% ====================================================================
%% Internal functions
%% ====================================================================


%%     inets:start(),
%%      {ok, Result} = httpc:request( post, {"http://192.168.0.100:12001/BGCModule.asmx",
%%                                           [{"Host" , "192.168.0.100"},
%%                                            {"SOAPAction", "http://192.168.0.100:12001/Login"},
%%                                            {"Authorization", "Basic c3lzOmNmOGE3MTg4NDM5NWMyMjNlNDFmMjg1N2MwNzgxZDA5"},
%%                                            {"te", "base64"}
%%                                           ],
%%                                           "text/xml;charset=utf-8",
%%                                           Data},
%%                                    [ {timeout, 50000} ], []
%%                                  ),
%%      io:format("Soap Response is ~p~n", [Result])
%%
