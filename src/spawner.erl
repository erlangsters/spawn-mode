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
-export([setup_spawn/3, setup_spawn/4, setup_spawn/5, setup_spawn/6]).

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
-type setup_fun() :: fun((pid(), reference()) -> term()).

%%
%% Spawn a process with a function.
%%
%% It spawns a process using the `erlang:spawn_opt/x` function. It honors the
%% spawn mode by translating it to the right spawn options.
%%
%% If the mode specifies a monitor link, the monitor reference is returned
%% alongside the process ID.
%%
-spec spawn(mode(), function()) -> spawn_return().
spawn(Mode, Fun) ->
    {Link, Options} = normalize_mode(Mode),
    spawn_opt(Fun, to_spawn_options(Link, Options)).

%%
%% Spawn a process with a function on a given node.
%%
%% See `spawn/2` for more information.
%%
-spec spawn(mode(), node(), function()) -> spawn_return().
spawn(Mode, Node, Fun) ->
    {Link, Options} = normalize_mode(Mode),
    spawn_opt(Node, Fun, to_spawn_options(Link, Options)).

%%
%% Spawn a process with a MFA.
%%
%% See `spawn/2` for more information.
%%
-spec spawn(mode(), module(), function(), [term()]) -> spawn_return().
spawn(Mode, Module, Function, Args) ->
    {Link, Options} = normalize_mode(Mode),
    spawn_opt(Module, Function, Args, to_spawn_options(Link, Options)).

%%
%% Spawn a process with a MFA on a given node.
%%
%% See `spawn/2` for more information.
%%
-spec spawn(mode(), node(), module(), function(), [term()]) -> spawn_return().
spawn(Mode, Node, Module, Function, Args) ->
    {Link, Options} = normalize_mode(Mode),
    spawn_opt(Node, Module, Function, Args, to_spawn_options(Link, Options)).

%%
%% Spawn an initialized process with a function.
%%
%% It spawns a process using the `erlang:spawn_opt/x` function and initializes
%% it using a setup function. It does the spawning with a temporary monitor
%% which allows you to catch any crash that might occur during the setup phase.
%% By the time this function returns, the spawn mode is honored.
%%
%% The process will be spawned using a monitor version of the spawn mode (in
%% order to preserve the process options that cannot be changed later). Then
%% the setup function will be called with the process ID and the monitor
%% reference. Whatever the setup function returns is returned by this function
%% after the process was adjusted according to the spawn mode.
%%
%% Note that you should provide a locking mechanism as the process should not
%% crash between the time the setup function returns and this function returns.
%%
-spec setup_spawn(mode(), function(), setup_fun()) ->
    {setup, term(), undefined | spawn_return()}.
setup_spawn(Mode1, Fun, Setup) ->
    spawn_with_setup(
        Mode1,
        fun(Mode2) -> spawner:spawn(Mode2, Fun) end,
        Setup
    ).

%%
%% Spawn an initialized process with a function on a given node.
%%
%% See `setup_spawn/3` for more information.
%%
-spec setup_spawn(mode(), node(), function(), setup_fun()) ->
    {setup, term(), undefined | spawn_return()}.
setup_spawn(Mode1, Node, Fun, Setup) ->
    spawn_with_setup(
        Mode1,
        fun(Mode2) -> spawner:spawn(Mode2, Node, Fun) end,
        Setup
    ).

%%
%% Spawn an initialized process with a MFA.
%%
%% See `setup_spawn/3` for more information.
%%
-spec setup_spawn(mode(), module(), function(), [term()], setup_fun()) ->
    {setup, term(), undefined | spawn_return()}.
setup_spawn(Mode1, Module, Function, Args, Setup) ->
    spawn_with_setup(
        Mode1,
        fun(Mode2) -> spawner:spawn(Mode2, Module, Function, Args) end,
        Setup
    ).

%%
%% Spawn an initialized process with a MFA on a given node.
%%
%% See `setup_spawn/3` for more information.
%%
-spec setup_spawn(mode(), node(), module(), function(), [term()], setup_fun()) ->
    {setup, term(), undefined | spawn_return()}.
setup_spawn(Mode1, Node, Module, Function, Args, Setup) ->
    spawn_with_setup(
        Mode1,
        fun(Mode2) -> spawner:spawn(Mode2, Node, Module, Function, Args) end,
        Setup
    ).


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

-spec spawn_with_setup(mode(), fun((mode()) -> spawn_return()), setup_fun()) ->
    {setup, term(), spawn_return()}.
spawn_with_setup(Mode, Spawn, Setup) ->
    % We cannot change the process options later on and therefore the strategy
    % is to make a "monitor variant" of the passed mode, then later we adjust
    % accordingly.
    {Link, Options} = normalize_mode(Mode),
    MonitorMode = {monitor, Options},

    % Spawn the process and execute the setup function.
    {Pid, Monitor1} = Spawn(MonitorMode),
    Value = Setup(Pid, Monitor1),

    % The setup function does not have to report whether the process has
    % terminated or not.
    case erlang:is_process_alive(Pid) of
        true ->
            % Remove the monitor and do the adjustments to honor the initial
            % spawn mode.
            erlang:demonitor(Monitor1),
            Return = case Link of
                no_link ->
                    Pid;
                link ->
                    link(Pid),
                    Pid;
                monitor ->
                    Monitor2 = erlang:monitor(process, Pid),
                    {Pid, Monitor2};
                {monitor, MonitorOptions} ->
                    Monitor2 = erlang:monitor(process, Pid, MonitorOptions),
                    {Pid, Monitor2}
            end,
            {setup, Value, Return};
        false ->
            % No adjustment to be made.
            erlang:demonitor(Monitor1),
            {setup, Value, undefined}
    end.

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
