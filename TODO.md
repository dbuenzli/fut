* TODO review 
** Arguments that return one of their argument w.r.t. abort. 
** double set on result, 
** check stop_wait when many can set.
** All user defined functions are trapped for exn. 
** Check what happens when trap raises.
** Is Fut.barrier really useful ? Maybe remove it and add a set 
   argument to Fut.fold.
* TODO check this bug  http://caml.inria.fr/mantis/view.php?id=5783
* TODO apply, queues could hold refs on options to abort if not 
  already scheduled.

* TODO test timers. 
* Forking. Introduce Backend.forked () that cleans up. 
