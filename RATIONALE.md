
Implementation wise, Fut implements a single-threaded cooperative
model with integrated IO multiplexing and FIFO work queues implemented
over a thread pool.

why, wHY, WHY ? 


Main reasons for Fut is disatisfaction with the algebra of Lwt's
combinators w.r.t to the treatement of exceptions and cancellation
aswell as with the concepts exposed. The following points highlight
some of the differences between Lwt and Fut. I don't have a precise
account for async (don't want to bring in the core library).

* Fut puts the emphasis on *values* rather than execution by using the
  concept of future and promises. Lwt center around the notion of
  thread. This is mainly a question of vocabulary, but vocabulary
  shapes our minds and the way we think about programs.

* Fut has a cooperative multitasking concurrency model implemented
  along Vouillon's lwt paper.

* Fut does not encourage the use of exceptions. If a future
  determination raises an exception, the exception is trapped by the
  runtime system and given to a global client-defined exception
  handler along with the backtrace. The state of the corresponding
  future is then set to be never determined. This state is also used
  for aborted futures. In lwt the exception itself becomes the
  (failed) thread state. In some sense there's only one "failed" state
  in Fut.

* Fut allows to abort any future (provided it's not already
  determined).  The state of an aborted future is set to never
  determine.  Lwt has both cancelable and non-cancelable threads and
  uses an exception for thread cancellation. Sometimes this may lead
  to surprising results e.g. `Lwt.pick [t, t']` may return a cancelled
  thread if `t` terminated and `t'` was cancelled. `Fut.pick f f'` is a
  never determined future only if both `f` and `f'` never determine
  (put differently, `Fut.never` is a neutral element for `Fut.pick`).

* Fut defines a simple denotational semantics for futures (a map from
  time to their state). The result of every combinator is defined in
  terms of this semantics. Equational reasoning is however hindered by
  the fact that some combinators perform side-effects on their
  arguments (e.g. `Fut.abort` and `Fut.pick`). But at least the semantics
  of the combinators is precisly defined.
   
* Fut tries to be minimalistic. I relies solely on the Unix and Thread
  module and provides no derived abstraction. Its runtime system is
  also simpler. Lwt has a much larger feature set, additional data
  structures, extension to Unix functionality and careful windows
  support (Fut relies on the Unix module for that).

* Fut does all its IO multiplexing via `select(2)`. This restriction
  may be lifted future versions. In fact there's an API to write your
  own backend.
