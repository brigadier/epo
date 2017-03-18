-module(epo_po).
-author("evgeny").
-include("epo_plural_rules.hrl").
-include("epo.hrl").
%% API
-export([create/4]).

-define(MAXW, 70).

header(Locale, App) ->
	HeaderMsg1 = [
		<<"">>,
		iolist_to_binary(io_lib:format(<<"Project-Id-Version: ~s\\n">>, [App]))
	],
	HeaderMsg = header_with_plural(HeaderMsg1, Locale),
	#porec{msgid = {undefined, <<"">>}, msgstr = HeaderMsg}.


create(File, Locale, App, Data) ->
	ok = filelib:ensure_dir(File),
	Header = header(Locale, App),
	NPlurals = nplurals(Locale),
	{ok, Handle} = file:open(File, [write, binary]),
	ok = dump(Handle, NPlurals, [Header | Data]),
	file:close(File),
	ok.

header_with_plural(Header, undefined) ->
	Header ++ [<<"Plural-Forms: nplurals=1; plural=1;\\n">>];
header_with_plural(Header, Locale) ->
	case plural_rule(Locale) of
		{_, S, _} -> Header ++ [iolist_to_binary(io_lib:format(<<"Plural-Forms: ~s\\n">>, [S]))];
		_ -> header_with_plural(Header, undefined)
	end.

nplurals(undefined) -> 1;
nplurals(Locale) ->
	case plural_rule(Locale) of
		{_, _, N} -> N;
		_ -> 1
	end.


plural_rule(Locale) ->
	case lists:keyfind(Locale, 1, ?PLURALS) of
		false ->
			case re:split(Locale, "_", [{return, binary}]) of
				[Locale2, _] -> plural_rule(Locale2);
				_ -> undefined
			end;
		{L, S, N} -> {L, S, N}
	end.



dump(_, _, []) -> ok;
dump(Handle, NPlurals, [Rec | Data]) ->
	ok = dump_rec(Handle, NPlurals, Rec),
	dump(Handle, NPlurals, Data).


dump_rec(Handle, NPlurals, #porec{msgid = {Context, MsgID}, msgstr = MsgStr, msgid_plural = MsgIDPlural, comments = Comments, msgstr_n = MsgStrN}) ->
	ok = dump_emptyline(Handle),
	ok = dump_comments(Handle, Comments),
	ok = dump_context(Handle, Context),
	ok = dump_msgid(Handle, MsgID),
	ok = dump_msgid_plural(Handle, MsgIDPlural),
	ok = dump_msgstrs(Handle, NPlurals, MsgIDPlural, MsgStr, MsgStrN).


dump_msgstrs(Handle, NPlurals, undefined, MsgStr, MsgStrN) when is_binary(MsgStr) ->
	dump_msgstrs(Handle, NPlurals, undefined, split(escape(MsgStr), ?MAXW), MsgStrN);
dump_msgstrs(Handle, _NPlurals, undefined, [MsgStr | Continuation], _MsgStrN) ->
	ok = file:write(Handle, [<<"msgstr \"">>, MsgStr, <<"\"\n">>]),
	dump_continuation(Handle, Continuation);

dump_msgstrs(Handle, NPlurals, _MsgIDPlural, _MsgStr, MsgStrN) ->
	lists:foldl(
		fun
			(I, []) ->
				ok = dump_msgstr_n(Handle, I, <<"">>),
				[];
			(I, [S | L]) ->
				ok = dump_msgstr_n(Handle, I, S),
				L
		end,
		MsgStrN,
		lists:seq(0, NPlurals - 1)
	),
	ok.




dump_msgstr_n(Handle, N, S) when is_binary(S) ->
	dump_msgstr_n(Handle, N, split(escape(S), ?MAXW));
dump_msgstr_n(Handle, N, [S | Continuation]) ->
	ok = io:fwrite(Handle, <<"msgstr[~B] \"~ts\"\n">>, [N, S]),
	dump_continuation(Handle, Continuation).


dump_continuation(Handle, L) ->
	lists:foreach(
		fun(S) ->
			ok = file:write(Handle, [<<"\"">>, S, <<"\"\n">>])
		end,
		L
	),
	ok.

dump_msgid_plural(_Handle, undefined) -> ok;
dump_msgid_plural(Handle, MsgIDPlural) when is_binary(MsgIDPlural) ->
	dump_msgid_plural(Handle, split(escape(MsgIDPlural), ?MAXW));
dump_msgid_plural(Handle, [MsgIDPlural | Continuation]) ->
	io:fwrite(Handle, <<"msgid_plural \"~ts\"\n">>, [MsgIDPlural]),
	dump_continuation(Handle, Continuation).

dump_msgid(Handle, MsgID) when is_binary(MsgID) ->
	dump_msgid(Handle, split(escape(MsgID), ?MAXW));
dump_msgid(Handle, [MsgID | Continuation]) ->
	io:fwrite(Handle, <<"msgid \"~ts\"\n">>, [MsgID]),
	dump_continuation(Handle, Continuation).

dump_context(_Handle, undefined) -> ok;
dump_context(Handle, Context) ->
	io:fwrite(Handle, <<"msgctxt \"~ts\"\n">>, [escape(Context)]).

dump_comments(_Handle, undefined) -> ok;
dump_comments(_Handle, []) -> ok;
dump_comments(Handle, [C | Comments]) ->
	file:write(Handle, [<<"#">>, escape(C), <<"\n">>]),
	dump_comments(Handle, Comments).

dump_emptyline(Handle) -> file:write(Handle, "\n").

escape(Bin) ->
	escape(Bin, [{<<"\t">>, <<"\\t">>}, {<<"\n">>, <<"\\n">>}, {<<"\"">>, <<"\\\"">>}]).
escape(Bin, []) -> Bin;
escape(Bin, [{K, V} | Rest]) ->
	escape(binary:replace(Bin, K, V, [global]), Rest).


split(Bin, NMax) when is_binary(Bin) ->
	L = unicode:characters_to_list(Bin, utf8),
	Len = length(L),
	split(L, Len, NMax, []).


split(List, Left, NMax, Acc) when Left =< NMax ->
	lists:reverse([unicode:characters_to_binary(List, utf8) | Acc]);
split(List, Left, NMax, Acc) ->
	{L1, L2} = lists:split(NMax, List),
	split(L2, Left - NMax, NMax, [unicode:characters_to_binary(L1, utf8) | Acc]).
