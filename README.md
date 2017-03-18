epo: gettext-like tool to i18n erlang applications
=========================
epo does two things:

* it extracts translatable strings from erlang source files (.erl) and erlydtl templates (.dtl) into .pot translation
template file. Both binaries and string constants are supported, though binaries should work slightly
faster at the translation time. Extraction is done by parsing AST of the code.

* it compiles .po translation files into a single erlang file, which can be used in erlang app to look
up i18n strings as fast as possible. 

* a satellite `epo_runtime` app allows you to use `_` function to mark translatable strings in the code and 
translate strings in runtime 


Epo should be used together with the 
[https://github.com/brigadier/epo_runtime/](https://github.com/brigadier/epo_runtime/) 
app, which contains a parse transform to transform `_` and `__` functions into calls to gettext.

Epo requires Erlang 17.0 to get built successfully, example app requires Erlang 18.0, as it has in
its' deps the master branch of the `Cowboy`
webserver. It can be trivially converted to be compatible with the earlier versions of Erlang though.

This app contains some code from the following apps:
 [https://github.com/erlydtl/erlydtl/](https://github.com/erlydtl/erlydtl/) - parsing of erlydtl templates
 [https://github.com/seriyps/gettexter](https://github.com/seriyps/gettexter) - parsing of the gettext 
 `Plural-forms` directive.


* Use with caution - at the moment it translates string  lists to
lists of the unicode codepoints, which can't be used in any
iolist-related functions. Do `unicode:characters_to_binary` before
passing such lists to any i/o.
 
=========================
 
### Build

To build Epo, run
    
    make escript
   
in this directory. This would create `epo` executable file.

### Workflow

* Copy Epo executable into the directory of the app you want to translate.
* Add `epo_runtime` as a dependency and add it to the list of applications in the `app.src` file
* Ensure your .erl files are compiled with the `epo_gettext` parse transform. For example, you can add
`-compile({parse_transform, epo_gettext}).` at the top of your .erl files
* Mark translatable strings with `_` (single underscore) or `__` (double underscore). See below for detailed description.
* Run `./epo scan [locales]`, for example  `./epo scan es ru` - this command will scan `./src/*` directory
for .erl files and all the directories for .dtl files and create the following files:
    `./src/[appname]_complied_po.erl` - a file which will contain strings lookup table. Think of it as kind of .mo file;
    `./priv/lang/[appname].pot` - a standard gettext translatons template file, to be used by `poedit` or other 
    software;
    `./priv/lang/[appname].[locale].po` - empty translation file for each of the locales
* Make translation files from the .pot file using `poedit` or other translation software. Save them in the
`./priv/lang/` directory, name them `[appname].[locale].po`
* Compile .po files into erlang file with `./epo compile`
* If you have in the source files of the app any strings (lists, not binaries!) which contain
 codepoints > 255 - ensure that `erlc` runs with `+{gettext, [appname]_compiled_po, unicode}` argument.
For example have `{erl_opts, [debug_info, {gettext, example_compiled_po, unicode}]}.` in the rebar.config. The in
and out strings will be converted from and to unicode, so you won't be able to
use them im any iolist-related functions and i/o. You'd have to convert all your
translated strings with `unicode:characters_to_binary`.

* If you don't have in the source files of the app any strings (lists, not binaries!) which contain codepoints > 255 AND
you don't want to have translated strings as unicode, ensure that `erlc` runs with `+{gettext, [appname]_compiled_po, list}` or
just with  `+{gettext, [appname]_compiled_po}` argument -
for example have `{erl_opts, [debug_info, {gettext, example_compiled_po, list}]}.` in the rebar.config. The in
and out strings will be converted from and to unicode, so you will be able to use them in  iolist-related functions
and in i/o directly.

* ensure `erlc` runs with `+{gettext, [appname]_compiled_po}` argument - for example for erlang.mk add the
following row in `Makefile`: `ERLC_OPTS += +'{gettext, [appname]_compiled_po}'`. Don't forget to replace 
`[appname]` with the actual name of your app.
* Make and run your app.

After you added new strings in the sources, just do again `./epo scan`, update .po files with poedit (Catalog -> Update from
POT file) and then run `./epo compile`.


### How to mark strings for translation

Epo should support all the standard features of gettext, such as, for example, context and plurals. Epo automatically
extracts strings from erlydtl (Django) `{% trans "" %}` and `{% blocktrans %} ... {% plural %} ... {% endblocktrans %}`
tags.

To mark the strings in .erl files use the following syntax:

* `__(S)` (double underscore) - mark the string for translation but don't wrap it in the translation function
* `_(S)` (single underscore) - mark the string for translation and wrap it in the translation function
* `_(S, Locale)` (single underscore) - mark the string for translation and wrap it in the translation function 
with the given locale
* `_(S, Plural, N)` (single underscore) - mark the string for translation, use Plural form for N > 1 and 
wrap it in the translation function
* `_(S, Plural, N, Locale)` (single underscore) - mark the string for translation, use Plural form for N > 1 and 
wrap it in the translation function with the given locale

`N` is int, `S` and `Plural` are binaries or strings. You can use other types, even callables, they will be translated
in runtime if the strings which they return are marked for translation somewhere else (otherwise epo won't be 
able to scan them). If locale is not supplied it will be extracted from the process dictionary with
the `erlang:get(locale)` call.

Everywhere instead of `S` you can use tuple `{Context, S}` - this would allow you to translate the same strings in the
different contexts, resulting in different translations.

For example, `_({<<"masculine">>, <<"A cat">>}, "Cats", N, <<"es">>)`


Epo does not support Erlang string escape sequences besides `\t`, `\n`, `\"`. Macroses are not supported.




### Example app
cd into `example` directory, make sure you have Erlang 18.0,
run `./rebar3 erlydtl compile && ./rebar3 release && _build/default/rel/example/bin/example console`,
 navigate to `http://localhost:8080`.
Most of the interesting stuff is in `rebar.config`, `./apps/example/src/example_handler.erl` and
 `./apps/example/priv/templates/test.dtl`.
 
 
 
### ToDo
* Switch for on/off unicode:characters_to_list in string translations.
* Support for Macros
* Customization, such as names of input and output files
* Extraction of comments for translators
* Autoupdating of .po files without poedit
* Better escaping


 