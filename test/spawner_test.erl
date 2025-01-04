%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, February 2024
%%
-module(spawner_test).
-include_lib("eunit/include/eunit.hrl").

% XXX: It does not test all the "spawn on another node" variants.
% XXX: Implementation could be more concise.

-define(SPAWN_MODE1, no_link).
-define(SPAWN_MODE2, link).
-define(SPAWN_MODE3, monitor).
-define(SPAWN_MODE4, {monitor, [{tag, 'OFF'}]}).
-define(SPAWN_MODE5, {no_link, [{priority, low}]}).
-define(SPAWN_MODE6, {{monitor, [{tag, 'OFF'}]}, [{priority, low}]}).

is_linked(Pid) ->
    case process_info(Pid, links) of
        {links, Links} ->
            lists:member(self(), Links);
        _ ->
            false
    end.

spawner_fun_test() ->
    Root = self(),
    Fun = fun() ->
        timer:sleep(100),
        Root ! '$test_loop'
    end,

    Pid1 = spawner:spawn(?SPAWN_MODE1, Fun),
    false = is_linked(Pid1),
    ok = receive
        '$test_loop' ->
            ok
    end,

    Pid2 = spawner:spawn(?SPAWN_MODE2, Fun),
    true = is_linked(Pid2),
    ok = receive
        '$test_loop' ->
            ok
    end,

    {Pid3, Monitor3} = spawner:spawn(?SPAWN_MODE3, Fun),
    false = is_linked(Pid3),
    ok = receive
        '$test_loop' ->
            ok
    end,
    ok = receive
        {'DOWN', Monitor3, process, Pid3, normal} ->
            ok
    end,

    {Pid4, Monitor4} = spawner:spawn(?SPAWN_MODE4, Fun),
    false = is_linked(Pid4),
    ok = receive
        '$test_loop' ->
            ok
    end,
    ok = receive
        {'OFF', Monitor4, process, Pid4, normal} ->
            ok
    end,

    Pid5 = spawner:spawn(?SPAWN_MODE5, Fun),
    false = is_linked(Pid5),
    {priority, low} = process_info(Pid5, priority),
    ok = receive
        '$test_loop' ->
            ok
    end,

    {Pid6, Monitor6} = spawner:spawn(?SPAWN_MODE6, Fun),
    false = is_linked(Pid6),
    {priority, low} = process_info(Pid6, priority),
    ok = receive
        '$test_loop' ->
            ok
    end,
    ok = receive
        {'OFF', Monitor6, process, Pid6, normal} ->
            ok
    end,

    ok.

spawner_mfa_test() ->
    Root = self(),
    meck:new(foo, [non_strict]),
    meck:expect(foo, bar, fun(Arg) ->
        timer:sleep(100),
        Root ! {'$test_loop', Arg}
    end),

    Pid1 = spawner:spawn(?SPAWN_MODE1, foo, bar, [42]),
    false = is_linked(Pid1),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,

    Pid2 = spawner:spawn(?SPAWN_MODE2, foo, bar, [42]),
    true = is_linked(Pid2),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,

    {Pid3, Monitor3} = spawner:spawn(?SPAWN_MODE3, foo, bar, [42]),
    false = is_linked(Pid3),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,
    ok = receive
        {'DOWN', Monitor3, process, Pid3, normal} ->
            ok
    end,

    {Pid4, Monitor4} = spawner:spawn(?SPAWN_MODE4, foo, bar, [42]),
    false = is_linked(Pid4),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,
    ok = receive
        {'OFF', Monitor4, process, Pid4, normal} ->
            ok
    end,

    Pid5 = spawner:spawn(?SPAWN_MODE5, foo, bar, [42]),
    false = is_linked(Pid5),
    {priority, low} = process_info(Pid5, priority),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,

    {Pid6, Monitor6} = spawner:spawn(?SPAWN_MODE6, foo, bar, [42]),
    false = is_linked(Pid6),
    {priority, low} = process_info(Pid6, priority),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,
    ok = receive
        {'OFF', Monitor6, process, Pid6, normal} ->
            ok
    end,

    meck:unload(foo),

    ok.

setup_spawn_fun_test() ->
    Root = self(),
    Fun = fun() ->
        Root ! {'$test_init', self()},
        timer:sleep(50),
        Root ! '$test_loop',
        timer:sleep(100),
        ok
    end,
    Setup = fun(Pid, Monitor) ->
        receive
            {'$test_init', Pid} ->
                ok;
            {'DOWN', Monitor, process, Pid, Reason} ->
                {not_ok, Reason}
        end
    end,

    {setup, ok, Pid1} = spawner:setup_spawn(?SPAWN_MODE1, Fun, Setup),
    false = is_linked(Pid1),
    ok = receive
        '$test_loop' ->
            ok
    end,

    {setup, ok, Pid2} = spawner:setup_spawn(?SPAWN_MODE2, Fun, Setup),
    true = is_linked(Pid2),
    ok = receive
        '$test_loop' ->
            ok
    end,

    {setup, ok, {Pid3, Monitor3}} = spawner:setup_spawn(?SPAWN_MODE3, Fun, Setup),
    false = is_linked(Pid3),
    ok = receive
        '$test_loop' ->
            ok
    end,
    ok = receive
        {'DOWN', Monitor3, process, Pid3, normal} ->
            ok
    end,

    {setup, ok, {Pid4, Monitor4}} = spawner:setup_spawn(?SPAWN_MODE4, Fun, Setup),
    false = is_linked(Pid4),
    ok = receive
        '$test_loop' ->
            ok
    end,
    ok = receive
        {'OFF', Monitor4, process, Pid4, normal} ->
            ok
    end,

    {setup, ok, Pid5} = spawner:setup_spawn(?SPAWN_MODE5, Fun, Setup),
    false = is_linked(Pid5),
    {priority, low} = process_info(Pid5, priority),
    ok = receive
        '$test_loop' ->
            ok
    end,

    % Test with a function that terminates early.
    TerminateFun = fun() -> ok end,
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE1, TerminateFun, Setup),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE2, TerminateFun, Setup),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE3, TerminateFun, Setup),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE4, TerminateFun, Setup),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE5, TerminateFun, Setup),

    % Test with a function that crashes.
    CrashFun = fun() -> throw(foobar) end,
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE1, CrashFun, Setup),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE2, CrashFun, Setup),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE3, CrashFun, Setup),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE4, CrashFun, Setup),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE5, CrashFun, Setup),

    ok.

setup_spawn_mfa_test() ->
    Root = self(),
    meck:new(foo, [non_strict]),

    meck:expect(foo, bar, fun(Arg) ->
        Root ! {'$test_init', self()},
        timer:sleep(50),
        Root ! {'$test_loop', Arg},
        timer:sleep(100),
        ok
    end),
    Setup = fun(Pid, Monitor) ->
        receive
            {'$test_init', Pid} ->
                ok;
            {'DOWN', Monitor, process, Pid, Reason} ->
                {not_ok, Reason}
        end
    end,

    {setup, ok, Pid1} = spawner:setup_spawn(?SPAWN_MODE1, foo, bar, [42], Setup),
    false = is_linked(Pid1),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,

    {setup, ok, Pid2} = spawner:setup_spawn(?SPAWN_MODE2, foo, bar, [42], Setup),
    true = is_linked(Pid2),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,

    {setup, ok, {Pid3, Monitor3}} = spawner:setup_spawn(?SPAWN_MODE3, foo, bar, [42], Setup),
    false = is_linked(Pid3),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,
    ok = receive
        {'DOWN', Monitor3, process, Pid3, normal} ->
            ok
    end,

    {setup, ok, {Pid4, Monitor4}} = spawner:setup_spawn(?SPAWN_MODE4, foo, bar, [42], Setup),
    false = is_linked(Pid4),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,
    ok = receive
        {'OFF', Monitor4, process, Pid4, normal} ->
            ok
    end,

    {setup, ok, Pid5} = spawner:setup_spawn(?SPAWN_MODE5, foo, bar, [42], Setup),
    false = is_linked(Pid5),
    {priority, low} = process_info(Pid5, priority),
    ok = receive
        {'$test_loop', 42} ->
            ok
    end,

    % Test with a function that terminates early.
    meck:expect(foo, bar, fun(_Arg) ->
        ok
    end),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE1, foo, bar, [42], Setup),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE2, foo, bar, [42], Setup),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE3, foo, bar, [42], Setup),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE4, foo, bar, [42], Setup),
    {setup, {not_ok, normal}, undefined} = spawner:setup_spawn(?SPAWN_MODE5, foo, bar, [42], Setup),

    % Test with a function that crashes.
    meck:expect(foo, bar, fun(_Arg) ->
        throw(foobar)
    end),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE1, foo, bar, [42], Setup),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE2, foo, bar, [42], Setup),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE3, foo, bar, [42], Setup),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE4, foo, bar, [42], Setup),
    {setup, {not_ok, {{nocatch, foobar}, _}}, undefined} = spawner:setup_spawn(?SPAWN_MODE5, foo, bar, [42], Setup),

    meck:unload(foo),

    ok.
