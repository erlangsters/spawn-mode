PROJECT = spawn_mode
PROJECT_DESCRIPTION = An Erlang process spawner API that solves a design fundamental issue.
PROJECT_VERSION = 0.1.0

TEST_DEPS = meck
dep_meck = git https://github.com/eproxus/meck 0.9.2

include erlang.mk
