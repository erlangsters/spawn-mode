PROJECT = spawn_mode
PROJECT_DESCRIPTION = An Erlang process spawner API that solves a design fundamental issue.
PROJECT_VERSION = 0.1.0

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck 1.0.0

include erlang.mk
