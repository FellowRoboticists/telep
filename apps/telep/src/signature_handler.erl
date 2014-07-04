-module(signature_handler).
-behavior(gen_server).

-record(keys, 
  { key_path,
    private_key,
    public_keys=[] }).

-export([ start_link/2 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ]).

start_link(KeyPath, PrivateKey) ->
  { ok, Pid } = gen_server:start_link(?MODULE, [ KeyPath, PrivateKey ], []),
  register(signature, Pid),
  { ok, Pid }.

init([ KeyPath, PrivateKey ]) ->
  { ok, #keys{key_path=KeyPath, private_key={PrivateKey, null} } }.

handle_call({ verify, SignedMessage }, _From, S=#keys{key_path=KeyPath, public_keys=PublicKeys}) ->
  [ Cmd, RobotName, Signature ] = string:tokens(SignedMessage, "|"),
  DigSig = base64:decode(Signature),
  Message = binary:list_to_bin(io_lib:format("~s|~s", [ Cmd, RobotName ])),
  case lists:keyfind(RobotName, 1, PublicKeys) of
    { RobotName, PublicKey } ->
      { reply, verify_signature(Message, DigSig, PublicKey), S };
    _ ->
      % Unable to find a key registered to the robot,
      % we need to attempt to load the key for it.
      case load_key_for_robot(RobotName, KeyPath) of
        { ok, Key } ->
          NewState = S#keys{public_keys=[{RobotName, Key}|PublicKeys]},
          { reply, verify_signature(Message, DigSig, Key), NewState };
        _ ->
          { reply, invalid, S }
      end
  end;

handle_call({ sign, MessageToSign }, _From, S=#keys{key_path=KeyPath, private_key={KeyName,null}}) ->
  FullPath = io_lib:format("~s/~s_private.pem", [ KeyPath, KeyName ]),
  case load_key(FullPath) of 
    { ok, Key } ->
      { reply, sign_message(MessageToSign, Key), S#keys{private_key={KeyName,Key}} };
    _ ->
      { reply, invalid, S }
  end;

handle_call({ sign, MessageToSign }, _From, S=#keys{private_key={_KeyName,Key}}) ->
  { reply, sign_message(MessageToSign, Key), S }.


handle_cast(_, State) ->
  { noreply, State }.

handle_info(_, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  State.

% Private Methods

sign_message(MessageToSign, Key) ->
  Signature = public_key:sign(binary:list_to_bin(MessageToSign), sha256, Key),
  io_lib:format("~s|~s", [ MessageToSign, base64:encode_to_string(Signature) ]).

verify_signature(Message, DigSig, Key) ->
  case public_key:verify(Message, sha256, DigSig, Key) of
    true -> { ok, Message };
    false -> { invalid }
  end.

load_key_for_robot(RobotName, KeyPath) ->
  FullPath = io_lib:format("~s/~s_public.pem", [ KeyPath, RobotName ]),
  load_key(FullPath).

load_key(FullPath) ->
  case filelib:is_regular(FullPath) of
    true ->
      { ok, PublicPemBin } = file:read_file(FullPath),
      [ KeyEntry ] = public_key:pem_decode(PublicPemBin),
      Key = public_key:pem_entry_decode(KeyEntry),
      { ok, Key };
    _ ->
      false
  end.

