-module(epo_xget).
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
	{ok, Forms} = epp_dodger:parse_file(File),
	Data = parse_forms(Forms, []),
	{ok, Data}.

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

from_form({application, {var, _, '__'}, [S_]}, Acc) ->
	{C, S} = extract(S_),
	store(#porec{msgid = S, msgstr = <<>>, msgctxt = C}, Acc);
from_form({application, {var, _, '_'}, [S_]}, Acc) ->
	{C, S} = extract(S_),
	store(#porec{msgid = S, msgstr = <<>>, msgctxt = C}, Acc);
from_form({application, {var, _, '_'}, [S_, _L]}, Acc) ->
	{C, S} = extract(S_),
	store(#porec{msgid = S, msgstr = <<>>, msgctxt = C}, Acc);
from_form({application, {var, _, '_'},  [S_, P_, _N]}, Acc) ->
	{C, S} = extract(S_),
	 P = extract_one(P_),
	store(#porec{msgid = S, msgstr = <<>>, msgid_plural = P, msgctxt = C}, Acc);
from_form({application, {var, _, '_'}, [S_, P_, _N, _L]}, Acc) ->
	{C, S} = extract(S_),
	P = extract_one(P_),
	store(#porec{msgid = S, msgstr = <<>>, msgid_plural = P, msgctxt = C}, Acc);
from_form(_Form, Acc) ->
	Acc.

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

extract({tree, tuple, _, [ContextTree, MsgTree]}) ->
	Context = extract_one(ContextTree),
	MsgID = extract_one(MsgTree),
	{Context, MsgID};
extract(Val) ->
	MsgID = extract_one(Val),
	{undefined, MsgID}.

extract_one({string, _, Val}) when is_list(Val) ->
	unicode:characters_to_binary(Val);
extract_one({tree, binary, {attr, _, _, _}, [{tree, binary_field, {attr, _, _, _}, {binary_field, {string, _, Val}, []}}]}) ->
	unicode:characters_to_binary(Val);
extract_one(_) ->
	error.
