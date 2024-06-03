%%
%% Copyright (c) 2024, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.txt file that
%% can be found at the root of the project directory.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>, February 2024
%%
-module(spawner).

-export_type([mode/0]).

-export([spawn/2, spawn/3, spawn/4, spawn/5]).
-export([setup/2]).

-type link() ::
    no_link |
    link |
    monitor |
    {monitor, [erlang:monitor_option()]}
.
-type option() ::
    {priority, Level :: erlang:priority_level()} |
    {fullsweep_after, Number :: pos_integer()} |
    {min_heap_size, Size :: pos_integer()} |
    {min_bin_vheap_size, VSize :: pos_integer()} |
    {max_heap_size, Size :: erlang:max_heap_size()} |
    {message_queue_data, MQD :: erlang:message_queue_data()} |
    {async_dist, Enabled :: boolean()}
.
-type mode() :: link() | {link(), [option()]}.

-type spawn_return() :: pid() | {pid(), reference()}.

-spec spawn(mode(), function()) -> spawn_return().
spawn(Mode, Fun) ->
    {Link, Options} = normalize_mode(Mode),
    spawn_opt(Fun, to_spawn_options(Link, Options)).

-spec spawn(mode(), node(), function()) -> spawn_return().
spawn(Mode, Node, Fun) ->
    {Link, Options} = normalize_mode(Mode),
    spawn_opt(Node, Fun, to_spawn_options(Link, Options)).

-spec spawn(mode(), module(), function(), [term()]) -> spawn_return().
spawn(Mode, Module, Function, Args) ->
    {Link, Options} = normalize_mode(Mode),
    spawn_opt(Module, Function, Args, to_spawn_options(Link, Options)).

-spec spawn(mode(), node(), module(), function(), [term()]) -> spawn_return().
spawn(Mode, Node, Module, Function, Args) ->
    {Link, Options} = normalize_mode(Mode),
    spawn_opt(Node, Module, Function, Args, to_spawn_options(Link, Options)).

-spec setup(pid(), mode()) -> spawn_return().
setup(_Pid, _Mode) ->
    % XXX: To be implemented.
    ok.

-spec is_process_option(erlang:monitor_option() | option()) -> boolean().
is_process_option({Name, _}) ->
    lists:member(Name, [
        priority, fullsweep_after, min_heap_size, min_bin_vheap_size,
        max_heap_size, message_queue_data, async_dist
    ]).

-spec normalize_mode(mode()) -> {link(), [option()]}.
normalize_mode(Mode) ->
    case Mode of
        {monitor, []} ->
            {{monitor, []}, []};
        {monitor, Options} ->
            % We need to resolve an ambiguity: options could be either the
            % monitor options of the process options. We look at the first
            % option to determine which one it is.
            IsProcessOption = is_process_option(hd(Options)),
            % lists:foreach(fun(Option) ->
            %     case is_process_option(Option) of
            %         IsProcessOption ->
            %             ok;
            %         _ ->
            %             erlang:error(badarg)
            %     end
            % end, tl(Options)),
            case IsProcessOption of
                true ->
                    {{monitor, []}, Options};
                false ->
                    {{monitor, Options}, []}
            end;
        {Link, Options} ->
            {Link, Options};
        Link ->
            {Link, []}
    end.

-spec to_spawn_options(link(), [option()]) -> erlang:spawn_opt_option().
to_spawn_options(Link, Options) ->
    case Link of
        no_link ->
            [];
        link ->
            [link];
        monitor ->
            [{monitor, []}];
        {monitor, MonitorOptions} ->
            [{monitor, MonitorOptions}]
    end ++ Options.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

is_process_option_test() ->
    ?assertEqual(false, is_process_option({tag, 'OFF'})),
    ?assertEqual(true, is_process_option({priority, low})),

    ok.

normalize_mode_test() ->
    ?assertEqual({no_link, []}, normalize_mode(no_link)),
    ?assertEqual({no_link, [{priority, low}]}, normalize_mode({no_link, [{priority, low}]})),

    ?assertEqual({link, []}, normalize_mode(link)),
    ?assertEqual({link, [{priority, low}]}, normalize_mode({link, [{priority, low}]})),

    ?assertEqual({monitor, []}, normalize_mode(monitor)),
    ?assertEqual({{monitor, []}, [{priority, low}]}, normalize_mode({monitor, [{priority, low}]})),
    ?assertEqual({{monitor, [{tag, 'OFF'}]}, []}, normalize_mode({monitor, [{tag, 'OFF'}]})),
    ?assertEqual({{monitor, [{tag, 'OFF'}]}, [{priority, low}]}, normalize_mode({{monitor, [{tag, 'OFF'}]}, [{priority, low}]})),

    % Mixing monitor and process options (we disabled this feature).
    % ?assertException(error, badarg, normalize_mode({monitor, [{tag, 'OFF'}, {priority, low}]})),
    % ?assertException(error, badarg, normalize_mode({monitor, [{priority, low}, {tag, 'OFF'}]})),
    % ?assertException(error, badarg, normalize_mode({{monitor, [{priority, low}]}, [{tag, 'OFF'}]})),

    ok.

to_spawn_options_test() ->
    ?assertEqual([], to_spawn_options(no_link, [])),
    ?assertEqual([{priority, low}], to_spawn_options(no_link, [{priority, low}])),

    ?assertEqual([link], to_spawn_options(link, [])),
    ?assertEqual([link, {priority, low}], to_spawn_options(link, [{priority, low}])),

    ?assertEqual([{monitor, []}], to_spawn_options(monitor, [])),
    ?assertEqual([{monitor, [{tag, 'OFF'}]}], to_spawn_options({monitor, [{tag, 'OFF'}]}, [])),
    ?assertEqual([{monitor, []}, {priority, low}], to_spawn_options(monitor, [{priority, low}])),

    ok.

-endif.
