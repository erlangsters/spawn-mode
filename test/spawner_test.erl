%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, February 2024
%%
-module(spawner_test).
-include_lib("eunit/include/eunit.hrl").

% XXX: It does not test all the spawn variants.

is_linked(Pid) ->
    case process_info(Pid, links) of
        {links, Links} ->
            lists:member(self(), Links);
        _ ->
            false
    end.

spawner_test() ->
    Root = self(),
    Fun = fun() ->
        timer:sleep(100),
        Root ! '$test_ok'
    end,

    Pid1 = spawner:spawn(no_link, Fun),
    false = is_linked(Pid1),
    ok = receive
        '$test_ok' ->
            ok
    end,

    Pid2 = spawner:spawn(link, Fun),
    true = is_linked(Pid2),
    ok = receive
        '$test_ok' ->
            ok
    end,

    {Pid3, Monitor3} = spawner:spawn(monitor, Fun),
    false = is_linked(Pid3),
    ok = receive
        '$test_ok' ->
            ok
    end,
    ok = receive
        {'DOWN', Monitor3, process, Pid3, normal} ->
            ok
    end,

    ok.
