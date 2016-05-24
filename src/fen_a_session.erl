%% @author zsoci
%% @doc @todo Add description to session.

-module(fen_a_session).
-behavour(psh).

-compile({parse_transform, ejson_trans}).

-include("fen_common.hrl").
-include_lib("psh/include/psh.hrl").
-include("fen_a_session.hrl").
%-include("opr_http.hrl").

-json_opt({type_field, [fen_session_data]}).
-json({fen_session_data,
       {binary, "user_id", [{default, <<"">>}]},
       skip,
       {number, "client_id", [{default, 0}]},
       {skip, [{default, false}]},
       skip,
       skip,
       skip,
       skip,
       skip,
       {skip, [{default, true}]},
       {binary, "access_token", [{default, <<"">>}]},
       {number, "token_expires", [{default, 0}]},
       {atom, "joined_to_parent", [{default, false}]},
       {number, "current_msg_nr", [{default, 0}]},
       {number, "delivered_msg_nr", [{default, 0}]}}).

%% ====================================================================
%% API functions
%% ====================================================================

-export([get_client_id/1,
         create_agent_identity/2,
         create/2,
         initstatus/3,
         call/3,
         call_local/3,
         execute/3,
         delete/1,
         encode_store/1,
         fetch_decode/1,
         terminate/1]).


delete(_WorkerIdentity = #agent_identity{}) ->
    ok.

%% create_agent_identity/2
%% ====================================================================
%% @doc form a unique agent identity for a session
%% @end
-spec create_agent_identity(UserId, ClientId) -> Result
          when  UserId :: binary(),
                ClientId :: undefined
                          | binary(),
                Result :: #agent_identity{}.
create_agent_identity(UserId, undefined) ->
    create_agent_identity(UserId, <<"0">>);
create_agent_identity(UserId, ClientId) ->
    #agent_identity{ path = ?FEN_AGENT_SESSION_NAME(UserId, ClientId) }.

%% call/3
%% <a> calls an mssession actor and waits for reply</a>
-spec call(UserId, ClientId, Message) -> Result when
          UserId :: binary,
          ClientId :: binary(),
          Message :: term(),
          Result :: term().
call(UserId, ClientId, Message) ->
    psh:call(create_agent_identity(UserId, ClientId), Message).

call_local(UserId, ClientId, Message) ->
    psh:call_local(create_agent_identity(UserId, ClientId), Message).

%% create/2
%% <a>Returns the initialised state record of the agent
-spec create(InitParams :: term(), State :: #agent_state{}) -> Result
          when Result :: #agent_state{}.
create({UserId, ClientId, AccessToken, TokenExpires}, State) ->
    WorkerData = #fen_session_data{user_id = UserId,
                                   client_id = ClientId,
                                   access_token = AccessToken,
                                   token_expires = TokenExpires},
    WState = psh:set_data(State, WorkerData),
    case check_if_joined_to_parent(UserId,
                                   WState) of
        {ok, ST} ->
            ST;
        _ ->
            WState
    end.

%% initstatus/2
%% <a>Returns {ok, data}
%% also refreshes the sessions as if a node crashes,
%% the state shall be retained</a>
-spec initstatus(Worker :: #agent_identity{},
                 Data :: undefined | #fen_session_data{},
                 WorkerStatus :: #agent_state{}) -> Result when
    Result :: {ok, Sessionstatus :: #fen_session_data{}} |
        error.
initstatus(#agent_identity{}, Data, _WorkerStatus = #agent_state{}) ->
    case Data of
        undefined ->
            {ok, #fen_session_data{}};
        ValidRecord ->
            {ok, ValidRecord}
    end.

%% execute/3
%% @doc <a>executes a message</a>
%% @end
-spec execute(Message, From, Status) -> Result when
    Message :: term(),
    From :: {Pid :: pid(), Tag :: term()},
    Status :: #agent_state{},
    Result :: {reply, Reply, NewState :: #agent_state{}}
            | {reply, Reply, NewState :: #agent_state{}, Timeout}
            | {reply, Reply, NewState :: #agent_state{}, hibernate}
            | {noreply, NewState :: #agent_state{}}
            | {noreply, NewState :: #agent_state{}, Timeout}
            | {noreply, NewState :: #agent_state{}, hibernate}
            | {stop, Reason, Reply, NewState :: #agent_state{}}
            | {stop, Reason, NewState :: #agent_state{}}.

execute({check_access_token, AccessToken}, _From,
        Status = #agent_state{agent_data = WorkerData =
                                                  #fen_session_data{}}) ->
    {reply, check_access_token(AccessToken, WorkerData), Status};

%% check for login. returns an {ok, AccessToken} or {error, badcredentials}
execute({check_credentials, UserId, Password},
         _From,
         Status = #agent_state{}) ->
    case em:authenticate(UserId, Password) of
        {error, missingagent} ->
           {stop, normal, {error, notfound}, Status};
        {error, Reason} ->
            {stop, normal, {error, Reason}, Status};
        {AccessToken, Expires, UserData, Host} ->
            check_device_id(UserId, AccessToken, Expires, UserData,
                            Host, Status)
    end;

execute({message, Msg}, _From,
        Status = #agent_state{agent_data = Data = #fen_session_data{}}) ->
    {_Index, NewStatus} = wrk_agent:add_message(Msg, Status),
    ReplyStatus =
        case Data#fen_session_data.getpid of
            undefined ->
                %% we are not in a get hook so just collect the message
                NewStatus;
            GSPid ->
                catch Data#fen_session_data.getwatchdog ! stop,
                {Messages, ReturnStatus} = wrk_agent:get_message_list(NewStatus),
                gen_server:reply(GSPid, {ok, Messages, undefined}),
                ReturnStatus
        end,
%    ?LOGFORMAT(debug, "Session execute message replying with state ~p", [ReplyStatus]),
    {noreply, ReplyStatus};

execute({gettimeout, UserId, DeviceId}, _From,
        Status = #agent_state{ agent_data =
                                    Data = #fen_session_data{}}) ->
    case (UserId =:= Data#fen_session_data.user_id) andalso
         (DeviceId =:= Data#fen_session_data.client_id) of
        true ->
            catch gen_server:reply(Data#fen_session_data.getpid, {ok, [], undefined});
        _ ->
            lager:warning("User or Device ID mismatch"),
            ok
    end,
    {noreply,
     Status#agent_state{agent_data =
                             Data#fen_session_data{getpid = undefined,
                                                   getwatchdog = undefined}}};

execute({<<"get">>, UserId, DeviceId, Message},
        From,
        Status = #agent_state{ agent_data = Data = #fen_session_data{}}) ->
    MsgSerial = maps:get(<<"MsgSerial">>, Message, 0),
    {MsgList, NewState} = wrk_agent:get_message_list(Status, MsgSerial),
    case MsgList of
        [] ->
            % @TODO check if the serial number of the last message delivered is less
            % than the serial number the entity have and if not, fetch the new messages from the entity
            case Data#fen_session_data.getpid of
                undefined ->
                    WatchDog = cu:watchdog_create({gettimeout, UserId,
                                                   binary_to_integer(DeviceId)},
                                                  10000),
%                                                  ?OPR_DEFAULT_GET_TIMEOUT),
                    NewData = Data#fen_session_data{getpid = From,
                                                    getwatchdog = WatchDog,
                                                    delivered_msg_nr = MsgSerial},
                    {noreply, Status#agent_state{agent_data = NewData}};
                _ ->
                    {noreply, NewStatus} = execute({gettimeout, UserId,
                                                    binary_to_integer(DeviceId)}, From, Status),
                    execute({<<"get">>, UserId, DeviceId, Message}, From, NewStatus)
            end;
        NewMsgList ->
            {reply, {ok, NewMsgList, undefined},
             Status#agent_state{agent_data =
                                     Data#fen_session_data{getpid = undefined,
                                                           delivered_msg_nr = MsgSerial}}}
    end;

execute({{get_entity, Id}, _UserId, _DeviceId, _Message},
        _From,
        Status = #agent_state{ agent_data = _Data = #fen_session_data{}}) ->
    JSON = em:get_entity_as_json(Id),
    {reply, {ok, JSON, undefined}, Status};

execute(stop, _From,  DCStatus = #agent_state{}) ->
    {stop, normal, DCStatus};
execute(Message, _From, DCStatus) ->
    lager:error("Unhandled message ~p~n\tfor domain ~p.", [Message, DCStatus]),
    {reply, {error, notimplemented}, DCStatus}.

terminate(DCStatus = #agent_state{agent_data = Data}) ->
%    lager:debug("Data ~p", [Data]),
    case Data#fen_session_data.getwatchdog =/= undefined of
        true ->
            Data#fen_session_data.getwatchdog ! stop,
            catch gen_server:reply(Data#fen_session_data.getpid,
                                   {error, "Internal Error"});
        _ ->
            ok
    end,
    DCStatus.

%% get_client_id/1
%% <a>Returns the last element as a number in a agent identity path</a>
-spec get_client_id(Worker :: #agent_identity{}) -> Result when
    Result :: non_neg_integer().
get_client_id(#agent_identity{path = Path}) ->
    binary_to_integer(
      lists:last(binary:split(Path, << ?PSH_AGENT_SEPARATOR >>, [global]))).

check_device_id(UserId,
                AccessToken,
                TokenExpires,
                UserData,
                Host,
                Status) ->
    case Status#agent_state.agent_data#fen_session_data.client_id of
        0 ->
            % give a correct clientid and quit
            create_new_client_id(UserId, AccessToken, TokenExpires,
                                 UserData, Host, Status);
        ClientId ->
            WorkerData = Status#agent_state.agent_data,
            NewWorkerData =
                WorkerData#fen_session_data{access_token = AccessToken,
                                            token_expires = TokenExpires},
            NewStatus = 
                case check_if_joined_to_parent(UserId,
                                               Status#agent_state{agent_data =
                                                                       NewWorkerData}) of
                    {ok, ST} ->
                        ST;
                    _ ->
                        Status#agent_state{agent_data = NewWorkerData}
                end,
            {reply, {ok, ClientId, AccessToken, UserData, Host}, NewStatus}
    end.

create_new_client_id(UserId, AccessToken, TokenExpires,
                     UserData, Host, Status) ->
    case em:get_new_client_id(UserId) of
        {ok, ClientId} ->
            create_new_client_agent(UserId, AccessToken, TokenExpires,
                                     UserData, ClientId, Host, Status);
        ELSE ->
            {stop, ELSE, ELSE, Status}
    end.


create_new_client_agent(UserId, AccessToken, TokenExpires,
                         UserData, ClientId, Host, Status) ->
    % create the agent for the new clientid and
    % @TODO set up a permanent parent child relationship
    Identity = opr_w_session:create_agent_identity(UserId,
                                                    integer_to_binary(ClientId)),
    case wrk_agent:create_agent(Identity, ?MODULE,
                                  {UserId, ClientId, AccessToken, TokenExpires},
                                  [{messaging, transient}]) of
        ok ->
            {stop, normal, {ok, ClientId, AccessToken, UserData, Host}, Status};
        {error, <<"failed">>} ->
            {stop, error, failed, Status};
        WAFIT ->
            {stop, WAFIT, WAFIT, Status}
    end.

check_access_token(TokenToCheck, #fen_session_data{access_token = Token,
                                                   token_expires = Expires}) ->
    case TokenToCheck =:= Token of
        true ->
            cu:now_usec() =< Expires;
        _ ->
            false
    end.

encode_store(Term) ->
    to_json(Term).

fetch_decode(Term) ->
    from_json(Term).

check_if_joined_to_parent(UserId,
                          State = #agent_state{agent_data = Data}) ->
    case Data#fen_session_data.joined_to_parent of
        true ->
            {ok, State};
        _ ->
            join_session_to_user(UserId, State)
    end.
%
join_session_to_user(UserId, State) ->
    case wrk_agent:join_to_parent(em:create_agent_identity(UserId),
                                   State#agent_state.identity,
                                   [],
                                   State) of
        {ok, NewState} ->
            OldData = NewState#agent_state.agent_data,
            NewData = OldData#fen_session_data{joined_to_parent = true},
            UpdatedState = NewState#agent_state{agent_data = NewData},
            case wrk_agent:store_agent_state(State#agent_state.identity,
                                               UpdatedState) of
                ok ->
                    {ok, UpdatedState};
                Else ->
                    Else
            end;
        Else ->
            Else
    end.

%%
%% execute({saveuserprofile, UserId = #user_credentials{}, EMail, Avatar, Nationality, Msg}, _From, DCStatus = #domain_state{identity = #domain_identity{application = Application},
%%                                                                                             appcallbackmodule = CallBackModule,
%%                                                                                             data = DCData = #mssession_dcdata{clientid = ClientId}}) ->
%%     case ClientId of
%%         0 ->
%%             {stop, error, {error, "Bad Cliend id (0)"}, DCStatus};
%%         _ClientId ->
%%             case msappuser:call({Application,
%%                                  UserId#user_credentials.name,
%%                                  {saveuserprofile, UserId, EMail, Avatar, Nationality, Msg}}) of
%%                 {ok, {MSUserData, AppUserData}} ->
%%                     {AppReplyData, NewAppData, NewDCStatus} = msapp:call_application_function(CallBackModule, saveuserprofile, {Msg}, AppUserData,
%%                                                                                             DCStatus#domain_state{data = DCData#mssession_dcdata{msgroupuserdata = MSUserData,
%%                                                                                                                                                  appuserdata = AppUserData}}),
%%                     {ReplyDCStatus, NewMsg} = msdomain:create_message("<bUpdateUserProfile>true</bUpdateUserProfile>",
%%                                                                 transient,
%%                                                                 NewDCStatus),
%%                     msdomain:send(msappuser:create_domain_identity(Application, string:to_lower(UserId#user_credentials.name)), NewMsg),
%%                     {reply, {ok, AppReplyData}, ReplyDCStatus#domain_state{data = DCData#mssession_dcdata{appuserdata = NewAppData,
%%                                                                                                        msgroupuserdata = MSUserData}}};
%%                 ELSE ->
%%                     {reply, ELSE, DCStatus}
%%             end
%%     end
%% ;
%%

