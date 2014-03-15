Taskies
=======
Taskies is a simple task manager.


Description
-----------
Write a `gen_server` with following interface:

* `schedule(Server, F, N).` Spawn a process with `F`. If `F`
  fails, restart it, but not more than `N` times.
  That schedule returns an `Id` (same between restarts).
  The call is a syncronous one.
  `Id = server:schedule(S, fun() -> do_something() end, 10).`
* `get_result(Server, Id)`. Return a status of the job `Id`.
  Not yet finished, failed too many times, wrong id etc.
* `cancel(Server, Id)` - forget the task about `Id`. async.
* ADDITIONAL STUFF. E.g. status, subscription, etc.
* Testing. Find a function that fails, but not always.

NOTE: Identificator shall be independent of the number of
restarts.


Design
------
Each restartable task is independent of all others.
The description suggests a single master server monitoring the status
of all tasks. That is a poor design choice, both semantically
(independent tasks are better to run independently) and operationally
(single master server is a SPOF and communicational bottleneck).

That's why an alternative design was chosen. Each task is
represented by a `gen_server`. That allows us to manage tasks
independently and concentrate on a single task lifecycle instead
of dealing with some aggregate state. A lot of subtle state-management
bugs are eliminated by design.
