* TODO review 
** Arguments that return one of their argument w.r.t. abort. 
** double set on result, 
** check stop_wait when many can set.
** All user defined functions are trapped for exn. 
** Check what happens when trap raises.
** Is Fut.barrier really useful ? Maybe remove it and add a set 
   argument to Fut.fold.
* Check this bug  http://caml.inria.fr/mantis/view.php?id=5783
* apply, queues could hold refs on options to abort if not 
  already scheduled.
* test timers. 
* use Printexc.raw_backtraces for the exception trap.
