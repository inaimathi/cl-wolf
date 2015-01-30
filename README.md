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

- think about deepcast! vs broadcast! (how do we tell when we want each? what are the actual pros/cons?)
- think about implementing a reader macro for those ascii-art diagram comments. They may in fact be compileable
- implement two versions of pulling reactors:
	1. queue intervening requests
	2. drop intervening requests
- figure out what to do about reactors that pull in a loop (or decide to disallow them)
- figure out some way to get return values out of a running system. Suggest tap parts that forward messages, but collect all throughput into some return structure
- more work on the ascii parsers
