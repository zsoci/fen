-define(FEN_DEFAULT_GET_TIMEOUT,50000).
-define(FEN_AGENT_SESSION_NAME(UserId, ClientId),
        << ?PSH_AGENT_NAME(?FEN_APP_NAME_B)/binary,
           ?PSH_AGENT_SEPARATOR,
           UserId/binary,
           ?PSH_AGENT_SEPARATOR,
           ClientId/binary >>).

-record(fen_session_data,{
                          user_id = <<"">> :: binary(),
                          password = <<"">> :: binary(),
                          client_id = 0 :: non_neg_integer(),
                          authenticated = false :: undefined,
                          userdata = undefined :: undefined,
                          getpid = undefined :: undefined,
                          getwatchdog = undefined :: undefined,
                          appuserdata = undefined :: undefined,
                          parentdomain = undefined :: undefined,
                          coldboot = true :: undefined,
                          access_token = <<"">> :: binary(),
                          token_expires = 0 :: non_neg_integer(),
                          joined_to_parent = false :: atom(),
                          current_msg_nr = 0 :: non_neg_integer(),
                          delivered_msg_nr = 0 :: non_neg_integer()
                         }
       ).
