-module(epo_dtl).
-author("evgeny").
-include("epo.hrl").
%% API
-export([file/1]).


file(File) ->
	process_flag(trap_exit, true),
	Parent = self(),
	Pid = spawn_link(
		fun() ->
			{ok, Data} = read_file(File),
			Parent ! {self(), data, Data}
		end
	),
	receive
		{Pid, data, Data} ->
			receive _ -> ok end,
			{ok, Data};
		X -> exit(X)
	end.
read_file(File) ->
	{ok, Data} = file:read_file(File),
	{ok, Tokens} = erlydtl_scanner:scan(Data),
	{ok, Forms} = erlydtl_parser:parse(Tokens),
	Result = parse_forms(Forms, []),
	{ok, Result}.


parse_forms([], Acc) ->
	Acc;


parse_forms([Form | Forms], Acc) when is_tuple(Form) ->
	Acc2 = from_form(Form, Acc),
	L = [X || X <- tuple_to_list(Form), (is_tuple(X) orelse is_list(X))],
	Acc3 = parse_forms(L, Acc2),
	parse_forms(Forms, Acc3);
parse_forms([Form | Forms], Acc) when is_list(Form) ->
	Acc2 = parse_forms(Form, Acc),
	parse_forms(Forms, Acc2);
parse_forms([_Form | Forms], Acc) ->
	parse_forms(Forms, Acc).


from_form({trans, {string_literal, _, MsgID}}, Acc) when is_list(MsgID)->
	store(#porec{msgid = string_literal(MsgID), msgstr = <<>>}, Acc);
from_form({trans, {string_literal, _, MsgID}, {string_literal, _, Context}}, Acc) when is_list(MsgID), is_list(Context) ->
	store(#porec{
		msgid = string_literal(MsgID),
		msgctxt = string_literal(Context),
		msgstr = <<>>
	}, Acc);

from_form({blocktrans, Params, Block, BlockPlural}, Acc) ->
	Context = proplists:get_value(context, Params, undefined),
	Context1 = context(Context),
	from_form(Block, BlockPlural, Context1, Acc);

from_form(_, Acc) ->
	Acc.

from_form(Block_, BlockPlural_, Context, Acc) ->
	Block = flatten_block(Block_),
	BlockPlural = flatten_block(BlockPlural_),

	store(#porec{msgid = Block, msgctxt = Context, msgstr = <<>>, msgid_plural = BlockPlural}, Acc).


context(undefined) -> undefined;
context({string_literal, _, Context}) when is_list(Context) ->  string_literal(Context).

flatten_block(undefined)  -> undefined;
flatten_block(Block) when is_list(Block) ->
	flatten_block(Block, <<>>).
flatten_block([], Acc) ->
	Acc;

flatten_block([{variable, {identifier, _, Name}} | Block], Acc) ->
	flatten_block(Block, <<Acc/binary, "{{ ", (atom_to_binary(Name, latin1))/binary, " }}">>);
flatten_block([{string, _, S} | Block], Acc) ->
	flatten_block(Block, <<Acc/binary, (unicode:characters_to_binary(S, latin1))/binary>>).

string_literal([$"| Rest]) ->
	unicode:characters_to_binary(unescape_string_literal(lists:droplast(Rest))).

store(#porec{msgctxt = Context, msgid = MsgID} = V, Acc) ->
	L = tuple_to_list(V),
	case lists:member(error, L) of
		true -> Acc;
		false ->
			Key = {Context, MsgID},
			case lists:keyfind(Key, 2, Acc) of
				false -> [V#porec{msgid = Key} | Acc];
				_ -> Acc
			end
	end.

%%unescape_string_literal is taken from erlydtl/erlydtl_compiler_utils.erl
unescape_string_literal(String) ->
    unescape_string_literal(String, [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\n | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\r | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\t | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).

%%
%%
%% fun() -> {ok, Mod} = erlydtl:compile_template("|{% blocktrans %} abs {{ val }} {% endblocktrans %}|", test), {ok, S} = Mod:render([{val, "12345"}], [{translation_fun, fun(Val, Locale) -> erlang:display(Val), "translated {{ val }}" end}]), io:format("~s~n", [S]) end().
%% test_dtl:render([], [{translation_fun, fun(Val, Locale) -> io:format("~n~p~n~n", [Val]) end}]).
%%
%% AAAA "This is the title."
%% AAAA undefined
%% AAAA "This string will have {{ value }} inside."
%% AAAA "\nThat will cost $ {{ amount }}.\n"
%% AAAA {"\nThere is only one {{ name }} object.\n",
%%       {"\nThere are {{ counter }} {{ name }} objects.\n",42}}
%% AAAA {"\nThat will cost $ {{ amount }} per year.\n",
%%       {"\nThat will cost $ {{ amount }} per {{ years }} years.\n",20}}
%% AAAA "Hi {{ name }}"

%%
%% test() ->
%% 	{ok, Data} = file:read_file("../../templates/test.dtl"),
%% %% 	Data = <<"<!DOCTYPE html>\n<html>\n<head>\n    <title></title>\n</head>\n<body>\n<div><div>\n<span>{% trans \"This is the title.\" %}</span>\n<span>{% trans myvar %}</span>\n<span>{% trans \"myvar\" noop %}</span>\n    {% for a in x %}\n<span>{% trans \"starting point\" as start %}</span>\n    {% endfor %}\n    </div>\n</div>\n{% blocktrans %}This string will have {{ value }} inside.{% endblocktrans %}\n\n{% blocktrans with amount=article.price %}\nThat will cost $ {{ amount }}.\n{% endblocktrans %}\n\n{% blocktrans with myvar=value|filter %}\nThis will have {{ myvar }} inside.\n{% endblocktrans %}\n{% blocktrans with book_t=book|title author_t=author|title %}\nThis is {{ book_t }} by {{ author_t }}\n{% endblocktrans %}\n\n{% blocktrans count counter=list|length %}\nThere is only one {{ name }} object.\n{% plural %}\nThere are {{ counter }} {{ name }} objects.\n{% endblocktrans %}\n\n\n{% blocktrans with amount=article.price count years=i.length %}\nThat will cost $ {{ amount }} per year.\n{% plural %}\nThat will cost $ {{ amount }} per {{ years }} years.\n{% endblocktrans %}\n\n{% blocktrans with name=user.username context \"greeting\" %}Hi {{ name }}{% endblocktrans %}\n\n{# {% blocktrans trimmed %} #}\n{#   First sentence. #}\n{#   Second paragraph. #}\n{# {% endblocktrans %} #}\n\n{# {% some_tag _(\"Page not found\") value|yesno:_(\"yes,no\") %} #}\n\n\n</body>\n</html>">>,
%% 	{ok, Tokens} = erlydtl_scanner:scan(Data),
%% 	{ok, Parsed} = erlydtl_parser:parse(Tokens),
%% 	parse_forms(Parsed, []).