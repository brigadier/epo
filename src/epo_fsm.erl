-module(epo_fsm).
-include("epo.hrl").


-export([parse/1]).

-record(state, {handle, owner, current_rec, acc = [], header = undefined}).



parse(File) ->
	process_flag(trap_exit, true),
	Parent = self(),
	Pid = spawn_link(
		fun() ->
			{ok, Handle} = file:open(File, [read, raw, binary, {read_ahead, 65535}]),
			{ok, #state{acc = Acc, header = H}} = wait_new(empty, #state{handle = Handle}),
			Parent ! {self(), po, H, Acc}
		end
	),
	receive
		{Pid, po, Header, Data} ->
			receive _ -> ok end,
			{ok, Header, Data};
		X -> exit(X)
	end.


wait_new(empty, #state{current_rec = undefined, handle = Handle} = State) ->
	wait_new(get_line(Handle), State);
wait_new({msgctxt, Context}, #state{current_rec = undefined, handle = Handle} = State) ->
	NewRec = #porec{msgctxt = Context},
	got_context(get_line(Handle), State#state{current_rec = NewRec});
wait_new({msgid, MsgID}, #state{current_rec = undefined, handle = Handle} = State) ->
	NewRec = #porec{msgid = MsgID},
	got_msgid(get_line(Handle), State#state{current_rec = NewRec});
wait_new(eof, State) ->
	{ok, State}.

got_context({msgid, MsgID}, #state{current_rec = CurrentRec, handle = Handle} = State) ->
	NewRec = CurrentRec#porec{msgid = MsgID},
	got_msgid(get_line(Handle), State#state{current_rec = NewRec}).

got_msgid({continuation, Continuation}, #state{current_rec = #porec{msgid = OldMsgId} = CurrentRec, handle = Handle} = State) ->
	NewRec = CurrentRec#porec{msgid = <<OldMsgId/binary, Continuation/binary>>},
	got_msgid(get_line(Handle), State#state{current_rec = NewRec});
got_msgid({msgid_plural, Plural}, #state{current_rec = CurrentRec, handle = Handle} = State) ->
	NewRec = CurrentRec#porec{msgid_plural = Plural},
	got_msgid_plural(get_line(Handle), State#state{current_rec = NewRec});
got_msgid({msgstr, MsgStr}, #state{current_rec = CurrentRec, handle = Handle} = State) ->
	NewRec = CurrentRec#porec{msgstr = MsgStr},
	got_msgstr(get_line(Handle), State#state{current_rec = NewRec}).

got_msgstr({continuation, Continuation}, #state{current_rec = #porec{msgstr = OldMsgStr} = CurrentRec, handle = Handle} = State) ->
	NewRec = CurrentRec#porec{msgstr = <<OldMsgStr/binary, Continuation/binary>>},
	got_msgstr(get_line(Handle), State#state{current_rec = NewRec});
got_msgstr(Event, #state{current_rec = CurrentRec, handle = Handle, acc = Acc, header = Header} = State) when Event == empty; Event == eof ->
	wait_new(get_line(Handle), State#state{current_rec = undefined, header = makeheader(CurrentRec, Header, Acc), acc = checkstore(CurrentRec, Acc)}).

got_msgid_plural({continuation, Continuation}, #state{current_rec = #porec{msgid_plural = OldMsgID} = CurrentRec, handle = Handle} = State) ->
	NewRec = CurrentRec#porec{msgid_plural = <<OldMsgID/binary, Continuation/binary>>},
	got_msgid_plural(get_line(Handle), State#state{current_rec = NewRec});
got_msgid_plural({msgstr_n, {N, Msg}}, #state{current_rec = #porec{msgstr_n = OldMsgStrNList} = CurrentRec, handle = Handle} = State) ->
	undefined = proplists:get_value(N, OldMsgStrNList, undefined),
	NewRec = CurrentRec#porec{msgstr_n = [{N, Msg} | OldMsgStrNList]},
	got_msgstr_n(get_line(Handle), State#state{current_rec = NewRec}).

got_msgstr_n({continuation, Continuation}, #state{current_rec = #porec{msgstr_n = [{N, OldVal} | OldMsgStrNList]} = CurrentRec, handle = Handle} = State) ->
	NewRec = CurrentRec#porec{msgstr_n = [{N, <<OldVal/binary, Continuation/binary>>} | OldMsgStrNList]},
	got_msgstr_n(get_line(Handle), State#state{current_rec = NewRec});
got_msgstr_n({msgstr_n, {N, Msg}}, #state{current_rec = #porec{msgstr_n = OldMsgStrNList} = CurrentRec, handle = Handle} = State) ->
	undefined = proplists:get_value(N, OldMsgStrNList, undefined),
	NewRec = CurrentRec#porec{msgstr_n = [{N, Msg} | OldMsgStrNList]},
	got_msgstr_n(get_line(Handle), State#state{current_rec = NewRec});
got_msgstr_n(Event, #state{current_rec = CurrentRec, handle = Handle, acc = Acc, header = Header} = State) when Event == empty; Event == eof ->
	wait_new(get_line(Handle), State#state{current_rec = undefined, header = makeheader(CurrentRec, Header, Acc), acc = checkstore(CurrentRec, Acc)}).


get_line(Handle) ->
	Line = file:read_line(Handle),
	line(maybestrip(Line)).

line(eof) ->
	eof;

line(<<>>) -> %%empty row
	empty;

line(<<$#, _Rest/binary>>) -> %%comment
	empty;

line(<<$\", Rest/binary>>) -> %%string continuation row
	{continuation, rm_last_quote(Rest)};

line(<<"msgid ", $\", Rest/binary>>) -> %%msgid row
	{msgid, rm_last_quote(Rest)};

line(<<"msgid_plural ", $\", Rest/binary>>) -> %%plural msg id
	{msgid_plural, rm_last_quote(Rest)};

line(<<"msgctxt ", $\", Rest/binary>>) -> %%msg context
	{msgctxt, rm_last_quote(Rest)};

line(<<"msgstr ", $\", Rest/binary>>) -> %%msgstr (translated)  row
	{msgstr, rm_last_quote(Rest)};

line(<<"msgstr[", Rest/binary>>) -> %%one of msgstr[0] translations
	{match, [BN, Text]} = re:run(Rest, "^(\\d+)\\]\\s\"(.*)$", [{capture, all_but_first, binary}]),
	N = binary_to_integer(BN),
	{msgstr_n, {N, rm_last_quote(Text)}}.



rm_last_quote(S) ->
	L = byte_size(S) - 1,
	<<Row:L/binary, $\">> = S,
	Row.



maybestrip(eof) -> eof;
maybestrip({ok, Bin}) -> strip(Bin).

strip(Bin) ->
	re:replace(Bin, "(^\\s+)|(\\s+$)", "", [global, {return, binary}]).

makeheader(#porec{msgid = <<>>, msgstr = MsgStr}, Header, []) ->
	case re:run(MsgStr, "Plural-Forms: (.*?)\\\\n", [{capture, all_but_first, list}]) of
		nomatch ->
			Header;
		{match, [Plurals]} ->
			%% noinspection ErlangUnresolvedFunction
			{ok, Tokens, _} = gettexter_plural_scanner:string(Plurals),
			%% noinspection ErlangUnresolvedFunction
			{ok, {plural_rule, NPlurals, _} = Tree} = gettexter_plural_parser:parse(Tokens),
			Expr = gettexter_plural:to_erlang_code(Tree, []),
			#poplural{expr = Expr, n = NPlurals}
	end;
makeheader(_, Header, _) ->
	Header.

checkstore(#porec{msgctxt = Context, msgid = Id, msgstr_n = Plurals} = Rec, Acc) ->
	Key = {Context, Id},
	case lists:keyfind(Key, 2, Acc) of
		false -> ok;
		_ ->
			exit({duplicate, lists:flatten(io_lib:format("~p", [Key]))})
	end,
	[Rec#porec{msgid = Key, msgstr_n = lists:reverse(Plurals)} | Acc].
