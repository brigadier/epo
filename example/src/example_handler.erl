%% coding: utf-8
-module(example_handler).
-compile({parse_transform, epo_gettext}).

%% API
-export([init/2]).

init(Req, Opts) ->
	#{lang := Locale} = cowboy_req:match_qs([{lang, [], <<"en">>}], Req),
	Trans1 =
		fun(I) ->
			{
				io_lib:format(_(<<"Hello ~B world!">>, <<"Hello ~B worlds!">>, I), [I]),
				io_lib:format(_(<<"Hello ~B world!">>, <<"Hello ~B worlds!">>, I, <<"es">>), [I]),
				io_lib:format(_(<<"Hello ~B world!">>, <<"Hello ~B worlds!">>, I, <<"ru">>), [I])
			}
		end,
	S1 = [Trans1(I) || I <- lists:seq(1, 5)],

	S2 = [__("House"), __({<<"masc">>, "cat"}), __({<<"fem">>, "cat"})],


	{ok, Compiled} = test_dtl:render(
		[{s1, S1}, {s2, S2}, {nn, lists:seq(1, 22)}, {lang, Locale}],
		[{translation_fun, fun translation_fun/2}, {locale, Locale}]
	),
	Req2 = cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/html">>}
	],
		[Compiled],
		Req),
	{ok, Req2, Opts}.


translation_fun({Val, {Plural_Val, Count}}, {Locale, Context}) ->
	_({Context, Val}, Plural_Val, Count, Locale);
translation_fun({Val, {Plural_Val, Count}}, Locale) ->
	_(Val, Plural_Val, Count, Locale);
translation_fun(Val, {Locale, Context}) ->
	_({Context, Val}, Locale);
translation_fun(Val, Locale) ->
	_(Val, Locale).
