# Spawn Mode

The spawn mode solves a fundamental recurrent issue in the design of your
APIs - the start functions.

There are many versions of the `spawn` functions (as of OTP 27, up to 21
variants) and exposing this variety to the end user is difficult. It solves the
problem by introducing a spawn mode which makes it easy to not tie a start
function to a single spawn variant.

The [OTPless behaviors](https://github.com/erlangsters/otpless-behaviors) make
use of this spawn mode and is a good example that shows how it was designed to
be used.

XXX: OTP 23 has introduced the `spawn_request/x` variation. It's not supported
     yet.

Written by Jonathan De Wachter (dewachter.jonathan@gmail.com) and released
under the MIT license.

## Getting started

When you implement a process behavior in a module, you will (almost always)
provide a start function which will invariably spawn a process at some point.

```erlang
start(foo, bar) ->
  % Do a few things here...
  Pid = spawn_link(fun loop/x).
  % ...and perhaps some more things here.
  {ok, Pid}.
```

However, by doing so, you restrict your process behavior to be started with
only one spawn method.

Because of that, some people will either leave it this way (which is fine if
the module is not shared, but then you have an incomplete API that will perhaps
need an update later) or resort to adding more start functions.

```erlang
-export([
    start/2,
    start_link/2,
    start_monitor/2
]).
```

This is how OTP behaviors solve this design issue (see the API of the
`gen_server` behavior for an example).

So, how is that bad? Well, those functions are not smart, they just wrap a
different spawning method, and can be very inconvenient to write (especially if
there are some initialization steps inside). Therefore they can be error-prone
and will pollute the API of your modules. But most annoyingly, it's a
unsatisfying solution that involves duplication, and which has to be replicated
for every single process behavior modules you will ever write. It's also
vulnerable to more potential spawn methods later introduced to the language.

This is when the spawn mode comes into play. Instead of tying your usual start
function to a given version of the spawn methods, you add a "spawn mode"
parameter to the start function.

```erlang
start(Mode, foo, bar) ->
  % Do the same few things here...
  Result = spawner:spawn(Mode, fun loop/x).
  % ...and the same some more things here.
  {ok, Result}.
```

With this revised version of your start function, you let the user choose how
the process implementing your behavior must actually be spawned.

```erlang
% Same as if the 'spawn' function is used.
{ok, Pid} = my_module:start(no_link, foo, bar),
% Same as if the 'spawn_link' function is used.
{ok, Pid} = my_module:start(link, foo, bar),
% Same as if the 'spawn_monitor' function is used.
{ok, {Pid, Monitor}} = my_module:start(monitor, foo, bar),
```

(Note that I've hidden the fact that spawning with a monitor will return both,
the pid and the monitor reference. It must be handled by your code, indeed.)

And the cherry on top of the cake is that it also lets the user also exploit
the more sophisticated spawn method.

```erlang
Pid = my_module:start({no_link, [{fullsweep_after, 2}]}, foo, bar).
```

The previous example is equivalent to spawning with the `spawn_opt` function
and passing `{fullsweep_after, 2}` as option while asking for no link nor
monitor.

Of course, all the spawn modes you would expect are available, see
the `spawner:spawn/x` functions. And last but not least, the spawn mode is
`spawner:mode/0` data type.

## Dealing with initializations

Next to the `spawner:spawn/x` functions, you will notice their
`spawner:setup_spawn/x` counterparts. They allow to safely initialize the
process **before** it can be returned.

Ultimately, this library should only be about the spawn mode and wrappers
around the built-in spawn functions that honor them. So why are we talking
about initialization ?

Well, it's frequent that right after spawning a new process, it will follow an
initialization sequence, and only once completed, the start function can
return.

```erlang
start(foo, bar) ->
  Pid = spawn(fun loop/x),
  % Do the initialization steps before returning.
  Pid ! {do_init, self()},
  receive
    {Pid, init_done} ->
      ok
  end,
  {ok, Pid}.
```

However, because the early sequence can go wrong, the implementation
of the start function is forced to spawn the process with a monitor.

```erlang
start(foo, bar) ->
  % We start the process with a monitor instead, then we do the initialization
  % steps.
  {Pid, Monitor} = spawn_monitor(fun loop/x),
  Pid ! {do_init, self()},
  receive
    {Pid, init_done} ->
      % Initialization was successful, the monitor can be removed.
      demonitor(Monitor),
      {ok, Pid};
    {'DOWN', Monitor, process, Pid, Reason} ->
      % Initialization went wrong, we return an error instead.
      {error, Reason}
  end.
```

How can we apply the same concept using the spawn mode then? With the
`spawner:setup_spawn/x` functions! You encapsulate the initialization part of
your code in a setup function, then let `setup_spawn/x` function do the rest.

```erlang
start(Mode, foo, bar) ->
  % The same initialization steps but in a function.
  Setup = fun(Pid, Monitor) ->
    Pid ! {do_init, self()},
    receive
      {Pid, init_done} ->
        ok;
      {'DOWN', Monitor, process, Pid, Reason} ->
        {not_ok, Reason}
    end
  end,
  % Spawning the process and initializing it.
  case spawner:spawn_setup(Mode, fun loop/x, Setup) of
    {setup, ok, Return} ->
      {ok, Return};
    {setup, {not_ok, Reason}, undefined} ->
      {error, Reason}
  end.
```

The provided setup function is passed the monitor which allows you to safely
initialize the process. Then by the time the start function returns, the spawn
mode is guaranteed to be honored.

## Bits of history

While I'm usually not a big fan of the API design decisions made for the OTP
(see my work on the [OTPless distribution](https://github.com/otpless-erlang)
Erlang with its [re-written behaviors](https://github.com/erlangsters/otpless-behaviors)),
there are often historical reasons for things being the way they are.

As of Erlang/OTP version 27, there are 5 types of the `spawn` function.

- `spawn/x` (4)
- `spawn_link/x` (4)
- `spawn_monitor/x` (4)
- `spawn_opt/x` (4)
- `spawn_request/x` (5)

With all their variations, that is no more than 21 functions! Even if they
share similarities, that feels heavy.

The `spawn_opt` functions were added in Erlang/OTP R6B (in 2000). The
`spawn_monitor` functions (and monitor functionalities in general) were added
in Erlang/OTP R10B (in 2004). And the `spawn_request` functions were added in
Erlang/OTP 23 (in 2020).

At the time when only `spawn/x` and `spawn_link/x` existed, adding a `start/x`
function for the former and a `start_link/x` function for the later may have
felt like an acceptable solution. But as the number of variants grows, it
becomes a tricky choice to make... either breaking backward compatibility for
an elegant solution, or keep adding a variant of the `start/x` function.

Perhaps that OTP team did not know the number of spawn functions would keep
growing.

Anyhow, they eventually decided to mitigate this by re-using the existing
"options" parameter (of the existing `start/x` functions) to pass the
spawn-related options.

```
start_opt() =
    {timeout, Timeout :: timeout()} |
    {spawn_opt, SpawnOptions :: [proc_lib:spawn_option()]} |
    enter_loop_opt()
```

It's an excerpt of the OTP documentation (the `gen_server` behavior).

XXX: Check factuality of the above infos.

## Using it in your project

With the **Rebar3** build system, add the following to the `rebar.config` file
of your project.

```
{deps, [
  {spawn_mode, {git, "https://github.com/erlangsters/spawn-mode.git", {tag, "master"}}}
]}.
```

If you happen to use the **Erlang.mk** build system, then add the following to
your Makefile.

```
BUILD_DEPS = spawn_mode
dep_spawn_mode = git https://github.com/erlangsters/spawn-mode master
```

In practice, you want to replace the branch "master" with a specific "tag" to
avoid breaking your project if incompatible changes are made.
