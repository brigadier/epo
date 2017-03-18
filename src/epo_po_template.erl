-module(epo_po_template).
-author("evgeny").

%% API
-export([erl/0]).


erl() ->
	{ok, [
<<"%% -*- coding: utf-8 -*-
-module(template).
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(nowarn_unused_record).

-record(porec2, {msgstr, msgstr_n = {}, n_max}).
-export([get_record/2, get_idx/2]).
-ignore_xref([get_record/2, get_idx/2]).
">>
	,
<<"get_idx(N, <<Locale2:2/binary, $_, _/binary>>) ->
	get_idx(N, Locale2);
get_idx(_, _) ->
	0.

get_record(Key, Locale) ->
	case Key of">>
	,
<<"		_ -> undefined
	end.

to_integer(true) -> to_integer(1);
to_integer(false) -> to_integer(0);
to_integer(N) when is_integer(N) -> N.

to_boolean(true) -> true;
to_boolean(false) -> false;
to_boolean(N) when N > 0 -> to_boolean(true);
to_boolean(N) when N == 0 -> to_boolean(false).
	">>]}.
