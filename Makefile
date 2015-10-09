PROJECT = norum
DEPS = sync lager

dep_lager = git https://github.com/basho/lager.git "3.0.0"

include erlang.mk
