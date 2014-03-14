-module(task_types).
-export_type([task_status/0
             ,task_id/0
             ,task_closure/0
             ,attempts_allowed/0
             ]).

% XXX: Record types are not exportable, so won't
% create 'task_spec' record (that would have a default value).

-type task_id() :: pid().
-type task_closure() :: fun(() -> any()).
-type attempts_allowed() :: pos_integer() | forever.
-type task_status() :: {working, non_neg_integer()}
                     | {succeeded, any()}
                     | fail_limit_reached.
