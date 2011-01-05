2011.01.05
==========

*Pluggable Type Systems by Bracha*
----------------------------------

[Gilad is right!](http://lambda-the-ultimate.org/node/1311)

*I wish Bracha would focus on the pluggable types rather than the Newspeak  --fogus*

Advantages of types:
- machine-readable documentation
- domain
- early error detection
- optimization

*What are the flaws in these arguments?  --fogus*

Role of type system

> The evaluation rules for typed λ calculi do not depend on their type rules. These 
> calculi almost always share the same evalu- ation rules - those of the untyped λ calculus. 
> The type system serves only to reject certain programs whose derivations might (or might not) “fail”.

The goal is to strive to separate the runtime from the type system.

> It isn’t practical to add all these analyzers into a language, which is why most of them languish as exotic 
> research projects. However, if the language provides a general framework for plugging in type systems, the 
> situation can change. Programmers might benefit from a host of analyses that integrate cleanly into their code.

How to integrate multiple type systems?  Clojure may have one answer:

> One approach would leverage off of the growing support for user-defined annotations (often referred 
> to as metadata) in programmming languages.

What about type inferencing?

> A better engineering approach is to implement type inference as a separate tool, available in the IDE.


*The Newspeak Programming Platform by Bracha*
---------------------------------------------

<http://www.cadence.com>

Classes are namespaces (modules)

> One can define an entire class library or framework as a set of classes nested within an outer class, and 
> then modify the library via inheritance, overriding classes defined within it. This is known as 
> **class hierarchy inheritance**

> In Newspeak, a top-level class constitutes a module definition. An instance of such a class is a module. 
> Top level classes do not have access to their surrounding scope, and are inherently stateless.

Object capabilities

Implemented with [Squeak Smalltalk](http://www.squeak.org/)

**Future**

- actors
- pluggable types

### L@@K

D. Ungar and R. Smith. SELF: The power of simplicity. In Proc. of the ACM Conf. on Object-Oriented Programming, Systems, Languages and Applications, Oct. 1987.

H. Ossher and W. Harrison. Combination of inheritance hierarchies. In Pro- ceedings OOPSLA ’92, ACM SIGPLAN Notices, pages 25–40, Oct. 1992. Published as Proceedings OOPSLA ’92, ACM SIGPLAN Notices, volume 27, number 10.

M. S. Miller. Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control. PhD thesis, Johns Hopkins University, Baltimore, Maryland, USA, May 2006.

B. B. Kristensen, O. L. Madsen, B. Møller-Pedersen, and K. Nygaard. The Beta Programming Language. In Research Directions in Object-Oriented Programming, pages 7–48. MIT Press, 1987.



2011.01.04
==========

*The First Report on Scheme Revisited* by Sussman and Steele*
-------------------------------------------------------------

what they thought Scheme would be...

> We thought that Scheme would turn out to be the next in a long series of programming languages that had been 
> designed at MIT over the course of 17 years. The principal themes of this series were complex data structures 
> with automatic pattern matching, and com- plex control structures with automatic backtracking.

Conniver vs. ACTORS

> Conniver made control points into first-class data, the actors model went to the logical extreme by making everything be first-class data.

their first attempt at an ACTORS language...

> Using MacLisp as a working environment, we wrote a tiny Lisp interpreter and then added mechanisms for creating actors and sending messages.

lexical scope...

> ... starting with a lexically scoped dialect of Lisp, because that seemed necessary to model the way names could refer to acquaintances in 
> PLASMA.  Lexical scoping would allow actors and functions to be created by almost identical mechanisms.

eureka!

> We concluded that actors and closures were effectively the same concept.

time and space (TODO: explore this notion further WRT Clojure, Lamport, Whitehead, Hewitt's notion of "Cells", etc.)

> Synchronization and mutual exclusion are matters of time, not of space, and are not properly addressed by lexical scoping, which 
> governs textual structures rather than the dynamics of execution.

### L@@K
- Victor Yngve’s COMIT (1962), a pattern-matching language intended for use in linguistic analysis
- In 1969, Carl Hewitt designed an extremely ambitious Lisp-like language for theorem- proving called Planner
    + pattern-directed procedure invocation and the use of automatic backtracking as an implementation mechanism for goal-directed search
- 1971... a subset of Plan- ner called Micro-Planner
- *Why Conniving Is Better Than Planning*
