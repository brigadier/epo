-module(epo).
-include("epo.hrl").
-include("epo_plural_rules.hrl").
-export([main/1]).

-define(MAGIC, "%%EPO COMPILED FILE\n").
main([]) ->
	io:format("Copy epo escript into the directory with the app you need to translate. Then use the following commands:~n~n"),
	io:format("./epo scan [locales] - makes .pot file. Also creates empty .po files and .erl file if they don't exist.~n"),
	io:format("     [locales] - space separated list of locales, such as en ru ru_RU. When .po files already exist,~n"),
	io:format("     locales will be extracted from names of .po files as well and added to the list of locales from the argument.~n~n"),

	io:format("./epo compile - compile .po files into erlang file with 'case Msg of' lookup.~n"),
	io:format("Bye.~n");
main(Args) ->
	Compile = lists:member("compile", Args),
	Scan = lists:member("scan", Args),
	if
		Compile -> compile(Args);
		Scan -> scan(Args);
		true -> main([])
	end.

scan(Args) ->
	{ok, App} = detect_app(),
	scan_app(Args, App).

scan_app(Args, App) ->
	ModuleName = module_name(App),
	SrcFileName = src_fname(ModuleName),

	SrcWildcard = src_wildcard(),
	DtlWildcard = dtl_wildcard(),

	SrcFiles = filelib:wildcard(SrcWildcard),
	Templates = filelib:wildcard(DtlWildcard),

	Locales = extend_locales([list_to_binary(L) || L <- Args, L =/= "scan"], existing_locales(App)),
	io:format("~p~n", [Locales]),
	io:format("Scan ~B locales~n", [length(Locales)]),

	ok = init_locales(App, Locales, SrcFiles, Templates),
	ok = maybe_file_exists(
		SrcFileName,
		fun(N) -> make_src_file(N, ModuleName, []) end
	),
	io:format("Done. Bye.~n").

extend_locales(L1, L2) ->
	lists:usort(L1 ++ L2).

existing_locales(App) ->
	PoWildcard = po_wildcard(App),
	PoFiles = filelib:wildcard(PoWildcard),
	[locale_from_file(PotFile) || PotFile <- PoFiles].



scan_src(SrcFiles) ->
	scan_src(SrcFiles, []).
scan_src([], Acc) ->
	{ok, Acc};
scan_src([{erl, F} | SrcFiles], Acc) ->
	{ok, Data} = epo_xget:file(F),
	scan_src(SrcFiles, join_strings(Acc, Data));
scan_src([{dtl, F} | SrcFiles], Acc) ->
	{ok, Data} = epo_dtl:file(F),
	scan_src(SrcFiles, join_strings(Acc, Data)).

join_strings(L1, L2) ->
	lists:ukeymerge(2, L1, lists:keysort(2, L2)).

init_locales(App, Locales, SrcFiles, Templates) ->
	{ok, Data} = scan_src([{erl, F} || F <- SrcFiles] ++ [{dtl, T} || T <- Templates]),
	ok = create_empty_pos(App, Locales, Data),
	create_pot_template(App, Data).

create_empty_pos(_App, [], _Data) ->
	ok;
create_empty_pos(App, [Locale | Locales], Data) ->
	File = po_fname(App, Locale),
	ok = maybe_file_exists(
		File,
		fun(N) -> create_empty_po(N, App, Locale) end
	),
	create_empty_pos(App, Locales, Data).

create_empty_po(File, App, Locale) ->
	ok = epo_po:create(File, Locale, App, []).


create_pot_template(App, Data) ->
	File = pot_fname(App),
	ok = epo_po:create(File, undefined, App, Data).


compile(_Args) ->
	{ok, App} = detect_app(),
	compile_app(App).


detect_app() ->
	[AppSrc|_] = filelib:wildcard("{src,ebin}/*.{app.src,app}"),
	{ok, [{_, App, _}]} = file:consult(AppSrc),
	BiNApp = erlang:atom_to_binary(App, latin1),
	io:format(<<"Found app: ~s~n">>, [App]),
	{ok, BiNApp}.


compile_app(App) ->
	PoWildcard = po_wildcard(App),
	ModuleName = module_name(App),
	SrcFileName = src_fname(ModuleName),
	PoFiles = filelib:wildcard(PoWildcard),
	io:format("~p~n", [PoFiles]),
	io:format("Found ~B po files~n", [length(PoFiles)]),

	ok = gettext(PoFiles, SrcFileName, ModuleName),

	io:format("Done. Bye.~n").

pot_fname(App) -> io_lib:format("./priv/lang/~s.pot", [App]).
po_fname(App, Locale) -> io_lib:format("./priv/lang/~s.~s.po", [App, Locale]).

po_wildcard(App) -> io_lib:format("./priv/lang/~s.*.po", [App]).

dtl_wildcard() -> "./templates/**/*.dtl".
src_wildcard() -> "./src/**/*.erl".

module_name(App) -> <<App/binary, "_compiled_po">>.
src_fname(Modulename) -> <<"./src/", Modulename/binary, ".erl">>.

maybe_file_exists(File, NotExists) ->
	maybe_file_exists(File, loud, NotExists).
maybe_file_exists(File, Noise, NotExists) ->
	ok = case filelib:is_regular(File) of
			 true when Noise == loud -> io:format("File \"~s\" already exists. Skipping.~n", [File]);
			 true when Noise == silent -> ok;
			 false -> NotExists(File)
		 end.

gettext(PoFiles, SrcFileName, ModuleName) ->
	{ok, Data} = read_po_files(PoFiles),
	ok = make_src_file(SrcFileName, ModuleName, Data).

read_po_files(PoFiles) ->
	process_flag(trap_exit, true),
	read_po_files(PoFiles, []).

read_po_files([], Acc) ->
	{ok, Acc};
read_po_files([PoFile | PoFiles], Acc) ->
	io:format("Parsing: ~s... ", [PoFile]),
	{ok, PoHeader, PoData} = epo_fsm:parse(PoFile),
	io:format("done~n"),
	read_po_files(PoFiles, [{locale_from_file(PoFile), PoHeader, PoData} | Acc]).

locale_from_file(File) ->
	{match, [Lang]} = re:run(File, ".*\\.(.+)\\.po$", [{capture, all_but_first, binary}]),
	Lang.

make_src_file(SrcFileName, ModuleName, Data) ->
	{ok, [Begin, Mid, End]} = read_template_file(),
	io:format("Opening file \"~s\"~n", [SrcFileName]),
	ok = check_magic(SrcFileName),
	{ok, Handle} = file:open(SrcFileName, [binary, write, {encoding, utf8}]),
	ok = mark_magic(Handle),
	ok = dump(fix_module_name(Begin, ModuleName), Handle),
	ok = dump_nlookup(Data, Handle),
	ok = dump(Mid, Handle),
	{ok, N} = dump_trans(Data, Handle),
	io:format("~B records saved.~n", [N]),
	ok = dump(End, Handle),
	io:format("Compiled ~B file(s) successfully.~n", [length(Data)]).


dump(Text, Handle) ->
	file:write(Handle, Text).

dump_trans(Data, Handle) ->
	{ok, DataDict} = to_single_list(Data),
	N = dict:fold(
		fun(Key, LocaleTuples, Acc) ->
			dump_trans_case(Key, LocaleTuples, Handle),
			Acc + 1
		end,
		0,
		DataDict
	),
	{ok, N}.

dump_trans_case(<<>>, _LocaleTuples, _Handle) ->
	ok;
dump_trans_case({_, <<>>}, _LocaleTuples, _Handle) ->
	ok;
dump_trans_case(Key, LocaleTuples, Handle) ->
	dump_trans_case_key(Key, Handle),
	dump_trans_locale_tuples(LocaleTuples, Handle).

dump_trans_case_key({undefined, Id}, Handle) ->
	io:fwrite(Handle, <<"\t\t<<\"~s\"/utf8>> ->~n\t\t\tcase Locale of\n">>, [Id]);
dump_trans_case_key({Context, Id}, Handle) ->
	io:fwrite(Handle, <<"\t\t{<<\"~s\"/utf8>>, <<\"~s\"/utf8>>} ->~n\t\t\tcase Locale of\n">>, [Context, Id]).

dump_trans_locale_tuples([], Handle) ->
	file:write(Handle, <<"\t\t\t_ -> undefined\n\t\t\tend;\n">>);
dump_trans_locale_tuples([{Locale, MsgStr, MsgStrN} | Locales], Handle) ->
	io:fwrite(Handle, <<"\t\t\t\t<<\"~s\">> -> ~s;~n">>, [Locale, format_msgstrs(MsgStr, MsgStrN)]),
	dump_trans_locale_tuples(Locales, Handle).

format_msgstrs(undefined, MsgStrN) ->
	N = length(MsgStrN),
	LN1 = [format_msgstr(MsgStr) || {_, MsgStr} <- MsgStrN],
	io_lib:format(<<"#porec2{msgstr = undefined, msgstr_n = {~s}, n_max = ~B}">>, [string:join(LN1, ","), N]);
format_msgstrs(MsgStr, []) ->
	Formatted = format_msgstr(MsgStr),
	io_lib:format(<<"#porec2{msgstr = ~s, msgstr_n = {}, n_max = 0}">>, [Formatted]).

format_msgstr(Bin) -> io_lib:format(<<"<<\"~s\"/utf8>>">>, [Bin]).

to_single_list(Data) ->
	to_single_list(Data, dict:new()).

to_single_list([], Dict) ->
	{ok, Dict};
to_single_list([{_Locale, _, []} | Data], Dict) ->
	to_single_list(Data, Dict);
to_single_list([{Locale, Header, [Rec | Records]} | Data], Dict) ->
	#porec{msgctxt = _Context, msgid = MsgID, msgstr = MsgStr, msgstr_n = MsgStrN} = Rec,
	Key = MsgID,
	Val = {Locale, MsgStr, MsgStrN},
	Dict2 = case dict:is_key(Key, Dict) of
				true ->
					dict:append(Key, Val, Dict);
				false ->
					dict:store(Key, [Val], Dict)
			end,
	to_single_list([{Locale, Header, Records} | Data], Dict2).


dump_nlookup([], _) ->
	ok;
dump_nlookup([{_Locale, undefined, _} | Data], Handle) ->
	dump_nlookup(Data, Handle);
dump_nlookup([{Locale, #poplural{expr = Expr} = _Header, _} | Data], Handle) ->
	file:write(Handle, io_lib:format("get_idx(N, <<\"~s\">>) ->~n\t", [Locale])),
	file:write(Handle, binary:replace(list_to_binary(Expr), <<"gettexter_plural:">>, <<>>, [global])),
	file:write(Handle, <<";\n">>),
	dump_nlookup(Data, Handle).

fix_module_name(Bin, ModuleName) ->
	binary:replace(Bin, <<"-module(template).">>, <<"-module('", ModuleName/binary, "').">>).

check_magic(SrcFileName) ->
	Exists = filelib:is_regular(SrcFileName),
	if
		Exists ->
			{ok, <<?MAGIC, _/binary>>} = file:read_file(SrcFileName);
		true ->
			ok
	end,
	ok.

mark_magic(Handle) ->
	file:write(Handle, ?MAGIC),
	file:sync(Handle),
	ok.



read_template_file() ->
	epo_po_template:erl().
