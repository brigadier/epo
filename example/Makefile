PROJECT = example
DEPS = cowboy erlydtl eunit_formatters epo_runtime

dep_cowboy = git https://github.com/ninenines/cowboy master
dep_epo_runtime = git https://github.com/brigadier/epo_runtime master

ERLC_OPTS += +'{gettext, example_compiled_po}'
RELX_URL = https://github.com/erlware/relx/releases/download/v3.4.0/relx

include erlang.mk
