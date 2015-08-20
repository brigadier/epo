-author("evgeny").
-ifndef(debug_print).
-define(debug_print, true).
-define(PRINT(Var), io:format("\nDEBUG: ~p:~p~n~p~n  ~p~n\n", [?MODULE, ?LINE, ??Var, Var])).
-define(PRINTS(Var), io:format("\nDEBUG: ~p:~p~n~p~n  ~s~n\n", [?MODULE, ?LINE, ??Var, Var])).
-define(DEBUG, io:format("\nDEBUG: ~p:~p~n\n", [?MODULE, ?LINE])).
-endif.

-ifndef(debug_notimplemented).
-define(debug_notimplemented, true).
-define(NOTIMPLEMENTED, exit({notimplemented, lists:flatten(io_lib:format("Function not implemented: [~p, ~p, ~p]", [?MODULE, ?FILE, ?LINE]))})).
-endif.


-record(porec, {msgid, msgid_plural, msgctxt, msgstr, msgstr_n = [], comments=[]}).
-record(poplural, {expr, n}).
