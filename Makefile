PROJECT = norum
DEPS = cowboy erlydtl sync lager

dep_erlydtl = git https://github.com/erlydtl/erlydtl.git "0.9.4"
dep_lager = git https://github.com/basho/lager.git "3.0.0"

include erlang.mk
