* Is Fut.barrier really useful ? Maybe remove it and add a set 
   argument to Fut.fold.
* Check this bug  http://caml.inria.fr/mantis/view.php?id=5783
* Fut.apply, queues could hold refs on options to abort if not 
  already scheduled.
* test timers. 
* use Printexc.raw_backtraces for the exception trap.
* Fut.firstl define semantics for empty list, also 
  underdefind with future that set to never determine.
* Fut.{first,pick} should we tag with `Left | `Right maybe less 
  Fut.map's would need to be introcuded ? But then Fut.{firstl,pickl}
  wouldn't tag.
* Fut.finally should we pass 'c ? 
* Fut.determine : 'a t -> until:'b t -> [ `Abort of 'b | `Det of 'a ]
* Fut.settle : 'a t -> until:'b t -> [ `Abort of 'b | `Never | `Det of 'a ]
