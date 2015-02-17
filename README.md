# CL-WOLF
###### Minimal Flow-Based Programming implementation

### So Here We Are...

For my purposes, I've concluded that the essential features of a flow-based programming system are

1. A uniformity of interface and message format for disparate parts
2. A focus on connections between black-box parts
3. The ability to control the destination of return values, if any

Together, those three will allow a great deal of flexibility in system design, while still mostly retaining diagrammatic representability. There is a great number of features I'm not concerned with, but that _you_ might be.

- **The sequencing of messages**. If a single part is connected to some number of other parts, their order of processing should not be assumed.
- **Uniformity of implementation**. There is no reason for all parts of the system to be written in the same language. I'm still working out the implications, but it should theoretically be possible to build completely heterogenous systems.
- **Full Diagrammatic Representability**. Working with systems like these has convinced me that THERE. WILL. ALWAYS. be edge cases. Conceptual little nooks and crannies where any given visual representation breaks down either to a) a massively repetitive set of components OR b) some special-purpose notation that describes said corner-case elegantly and correctly, but that implicitly assumes familiarity in the reader by hiding certain information. Neither of these is acceptable to me. So, in the absence of acceptable solutions, I'll take option c) diagrammatic representation where it helps, coupled with textual representation anywhere else.

### TODO

- implement two versions of pulling reactors:
	1. queue intervening requests
	2. drop intervening requests
- figure out what to do about reactors that pull in a loop (or decide to disallow them)
- figure out what to do about the scheduler
	- we want the server example to work. Should I just bite the bullet and use libevent/libev bindings?

##### Module-related

- re-jig module a bit to allow child parts to be passed as arguments to containers.
	- change the dependency finder correspondingly to ignore those requirements
- as it stands, a container doesn't guarantee consistency of its part tree
	- this is because we designate child parts by factory call
	- if we wanted to, we could additionally start designating child parts by hash (since these hashes would then be part of the container body, the containers' hash now guarantees its full tree)
	- this might have a downside for automated regression testing 
- think seriously about how a module server would look
	- you may end up replacing some local module stuff with calls out to the local server
	- this might be a better fit for a separate project `revres-wolf` or something, in keeping with the theme
- do we want dependencies to be specified by `hash`, or `name`?
	- one of our use cases is going to be automatically testing parts against new versions of their dependencies.
	- to be fair, thanks to the `fact-base` back-end, it's still possible to do that with a `hash` association.
- publish the module to a remote server
	- need some kind of user tag, so that we can identify later versions of the same part
	- modules should automatically be published locally if the local server is running
- compute inputs and outputs from the body
	- if the part is a container, easy peasy (same as the expansion macro)
	- otherwise try to infer from `in!` and `out!` forms
		- this is imperfect at best, and is impossible in general for push-reactors (even discounting `macrolet` tricks)
			- for example `(reactor
				             (let ((name (symbol-name tag)))
				               (cond 
				                 ((name (starts-with-subseq "rel"))
					              (do stuff))
				                 ((string= name "foobar")
					              (do other stuff))
				                 ((name (ends-with-subseq "ing"))
					              (do a third thing)))))`
			- do we just give up on push-reactors, or try to handle cases where inference is possible?

##### Ascii-related
- directional triggering bug (found by Dann)

\__     \__
\__  vs  \_

- one proposed fix: only connect `down-(right|left)`, rather than `(or down down-(right|left))`

- self output bug

  ---> countdown ---> printer
   |             \_-> decrement -
   |____________________________/

that line coming out of decrement ends up translating into a `(self :out)` connection, even though it isn't the intention.

