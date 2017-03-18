%% coding: utf-8
-module(example_handler).
-compile({parse_transform, epo_gettext}).

%% API
-export([init/2]).

init(Req, Opts) ->
	#{lang := Locale} = cowboy_req:match_qs([{lang, [], <<"en">>}], Req),
%%	erlang:put(locale, Locale),

	%% use iolist_to_binary instead of iolists
	Trans1 =
		fun(I) ->
			{
				iolist_to_binary(io_lib:format(_(<<"Hello ~B world!">>, <<"Hello ~B worlds!">>, I), [I])),
				iolist_to_binary(io_lib:format(_(<<"Hello ~B world!">>, <<"Hello ~B worlds!">>, I, <<"es">>), [I])),
				iolist_to_binary(io_lib:format(_(<<"Hello ~B world!">>, <<"Hello ~B worlds!">>, I, <<"ru">>), [I]))
			}
		end,
	S1 = [Trans1(I) || I <- lists:seq(1, 5)],
	S2 = [__("House"), __({<<"masc">>, "cat"}), __({<<"fem">>, "cat"})],

	%%noinspection ErlangUnresolvedFunction
	{ok, Compiled} = test_dtl:render(
		[{s1, S1}, {s2, S2}, {nn, lists:seq(1, 22)}, {lang, Locale}],
		[{translation_fun, fun translation_fun/2}, {locale, Locale}]
	),
	Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, [Compiled], Req),
	{ok, Req2, Opts}.

%% use unicode:characters_to_binary as it is not possible to use unicode data with code points >= 256
%% in iolist_... functions.
translation_fun({Val, {Plural_Val, Count}}, {Locale, Context}) ->
	unicode:characters_to_binary(_({Context, Val}, Plural_Val, Count, Locale));
translation_fun({Val, {Plural_Val, Count}}, Locale) ->
	unicode:characters_to_binary(_(Val, Plural_Val, Count, Locale));
translation_fun(Val, {Locale, Context}) ->
	unicode:characters_to_binary(_({Context, Val}, Locale));
translation_fun(Val, Locale) ->
	unicode:characters_to_binary(_(Val, Locale)).


