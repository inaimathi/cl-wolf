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

- start getting make-reactor/make-deactor/make-container to track their input and output ports
- think about deepcast! vs broadcast! (and actually, what we've got at the moment is deepcast!, so it's misnamed to boot)
- think about getting output structures together. Something that lets a reactor finish out what it's doing before the next thing runs (right now, we've got the curious situation where "broad"cast targets get to move ahead of their parent)
- since you're already doing macroexpansion-time processing, you may as well have only one `reactor` primitive, and make the decision of whether it's a `reactor` or `deactor` based on whether there are any `get!` calls.
- think about implementing a reader macro for those ascii-art diagram comments. They may in fact be compileable
