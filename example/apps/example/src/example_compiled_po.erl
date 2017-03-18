%%EPO COMPILED FILE
%% -*- coding: utf-8 -*-
-module('example_compiled_po').
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(nowarn_unused_record).

-record(porec2, {msgstr, msgstr_n = {}, n_max}).
-export([get_record/2, get_idx/2]).
-ignore_xref([get_record/2, get_idx/2]).
get_idx(N, <<"ru">>) ->
	to_integer(case
			      to_boolean(to_boolean(to_integer(to_integer(N)
														    rem
														    to_integer(10))
											==
											to_integer(1))
							    andalso
							    to_boolean(to_integer(to_integer(N)
														      rem
														      to_integer(100))
											  =/=
											  to_integer(11)))
				of
			      true -> 0;
			      false ->
				  case
				    to_boolean(to_boolean(to_boolean(to_integer(to_integer(N)
																		      rem
																		      to_integer(10))
															  >=
															  to_integer(2))
											      andalso
											      to_boolean(to_integer(to_integer(N)
																			rem
																			to_integer(10))
															    =<
															    to_integer(4)))
								  andalso
								  to_boolean(to_boolean(to_integer(to_integer(N)
																			rem
																			to_integer(100))
															    <
															    to_integer(10))
												orelse
												to_boolean(to_integer(to_integer(N)
																			  rem
																			  to_integer(100))
															      >=
															      to_integer(20))))
				      of
				    true -> 1;
				    false -> 2
				  end
			    end);
get_idx(N, <<"es_BO">>) ->
	to_integer(to_integer(N)
			      =/= to_integer(1));
get_idx(N, <<"es">>) ->
	to_integer(to_integer(N)
			      =/= to_integer(1));
get_idx(N, <<Locale2:2/binary, $_, _/binary>>) ->
	get_idx(N, Locale2);
get_idx(_, _) ->
	0.

get_record(Key, Locale) ->
	case Key of		<<"Hello"/utf8>> ->
			case Locale of
				<<"ru">> -> #porec2{msgstr = <<"Привет"/utf8>>, msgstr_n = {}, n_max = 0};
				<<"es_BO">> -> #porec2{msgstr = <<""/utf8>>, msgstr_n = {}, n_max = 0};
				<<"es">> -> #porec2{msgstr = <<"Hola"/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		{<<"second"/utf8>>, <<"Hello world!"/utf8>>} ->
			case Locale of
				<<"ru">> -> #porec2{msgstr = undefined, msgstr_n = {<<"Привет мой {{ n }} мир!"/utf8>>,<<"Привет {{ n }} моих мира!"/utf8>>,<<"Привет {{ n }} моих миров!"/utf8>>}, n_max = 3};
				<<"es">> -> #porec2{msgstr = undefined, msgstr_n = {<<"Hola mi mundo!"/utf8>>,<<"Hola mis {{ n }} mundos!"/utf8>>}, n_max = 2};
			_ -> undefined
			end;
		{<<"masc"/utf8>>, <<"cat"/utf8>>} ->
			case Locale of
				<<"ru">> -> #porec2{msgstr = <<"кот"/utf8>>, msgstr_n = {}, n_max = 0};
				<<"es_BO">> -> #porec2{msgstr = <<""/utf8>>, msgstr_n = {}, n_max = 0};
				<<"es">> -> #porec2{msgstr = <<"El gato"/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Hello ~B world!"/utf8>> ->
			case Locale of
				<<"ru">> -> #porec2{msgstr = undefined, msgstr_n = {<<"Привет ~B мир!"/utf8>>,<<"Привет ~B мира!"/utf8>>,<<"Привет ~B миров!"/utf8>>}, n_max = 3};
				<<"es_BO">> -> #porec2{msgstr = undefined, msgstr_n = {<<""/utf8>>,<<""/utf8>>}, n_max = 2};
				<<"es">> -> #porec2{msgstr = undefined, msgstr_n = {<<"Hola ~B mundo!"/utf8>>,<<"Hola ~B mundos!"/utf8>>}, n_max = 2};
			_ -> undefined
			end;
		<<"Hello world!"/utf8>> ->
			case Locale of
				<<"ru">> -> #porec2{msgstr = undefined, msgstr_n = {<<"Привет {{ n }} мир!"/utf8>>,<<"Привет {{ n }} мира!"/utf8>>,<<"Привет {{ n }} миров!"/utf8>>}, n_max = 3};
				<<"es_BO">> -> #porec2{msgstr = undefined, msgstr_n = {<<""/utf8>>,<<""/utf8>>}, n_max = 2};
				<<"es">> -> #porec2{msgstr = undefined, msgstr_n = {<<"Hola mundo!"/utf8>>,<<"Hola {{ n }} mundos!"/utf8>>}, n_max = 2};
			_ -> undefined
			end;
		<<"House"/utf8>> ->
			case Locale of
				<<"ru">> -> #porec2{msgstr = <<"Дом"/utf8>>, msgstr_n = {}, n_max = 0};
				<<"es_BO">> -> #porec2{msgstr = <<""/utf8>>, msgstr_n = {}, n_max = 0};
				<<"es">> -> #porec2{msgstr = <<"La casa"/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		{<<"fem"/utf8>>, <<"cat"/utf8>>} ->
			case Locale of
				<<"ru">> -> #porec2{msgstr = <<"кошка"/utf8>>, msgstr_n = {}, n_max = 0};
				<<"es_BO">> -> #porec2{msgstr = <<""/utf8>>, msgstr_n = {}, n_max = 0};
				<<"es">> -> #porec2{msgstr = <<"La gata"/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		_ -> undefined
	end.

to_integer(true) -> to_integer(1);
to_integer(false) -> to_integer(0);
to_integer(N) when is_integer(N) -> N.

to_boolean(true) -> true;
to_boolean(false) -> false;
to_boolean(N) when N > 0 -> to_boolean(true);
to_boolean(N) when N == 0 -> to_boolean(false).
	