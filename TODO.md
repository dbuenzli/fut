* Backend interface: settle FD and Backend action interface.
* Backend interface: introduce an Unsupported exception.
  given to the trap, for backend usage.
* Folds 
  Fut.fold : ?dets:bool -> ('a -> 'b -> 'a) -> 'a -> 'b t list -> 'a t
  Fut.fold_sets : ('a -> 'b option -> 'a) -> 'a -> 'b t list -> 'a t
 
* Check this bug  http://caml.inria.fr/mantis/view.php?id=5783
* Fut.apply, queues could hold refs on options to abort if not 
  already scheduled.
* Fut.firstl define semantics for empty list, also 
  underdefind with future that set to never determine.
* Fut.{first,pick} should we tag with `Left | `Right maybe less 
  Fut.map's would need to be introcuded ? But then Fut.{firstl,pickl}
  wouldn't tag.

* select(2) backend is not forkable yet because of the self-pipe.
  Should we have a Runtime.forked () call ?
  
* Fut.finally should we pass 'c ? 

* Fut.determine : 'a t -> until:'b t -> [ `Abort of 'b | `Det of 'a ]
  (the name should reflect that we give a chance to determine before being
   violent)
  
* Fut.settle : 'a t -> until:'b t -> [ `Abort of 'b | `Never | `Det of 'a ]
  (the name should reflect that we give a chance to set before being 
   violent.) 
  
* Fut.pair : 'a t -> 'b t -> ('a * 'b) t ? 
