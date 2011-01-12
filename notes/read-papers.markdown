2011.01.12
==========

*Language Virtualization for Heterogeneous Parallel Computing by Chafi, DeVito, Odersky, ...*
---------------------------------------------------------------------------------------------

*The need for DSLs*

> One way to capture application-specific knowledge for a whole class of applications and simplify application 
> development at the same time is to use a domain specific language (DSL). A DSL is a concise programming 
> language with a syntax that is designed to naturally express the semantics of a narrow problem domain

*The following is incorrect (or maybe just not specific enough)*

> Using DSLs naturally divides the application development process into two phases. First, a DSL developer 
> designs a DSL for a specific domain. Then, a much larger number of domain experts make use of the DSL to 
> develop applications.

This approach is a recipe for failure.  The domain experts **must** be involved in the design of the
DSL.  The DSL developer is a fool in the domain by comparison.  But they do say:

> The ideal DSL developer would be a domain expert, a parallelism expert, and a language and compiler 
> expert. Such developers are rare and so there is a need to simplify the process of developing DSLs for 
> parallelism.

Uhhhhh. What?  Maybe multiple people would work too. You think?

Argh!  They list numerous properties of virtualizable languages (langs facilitating internal DSLs along the
same footing as external DSLs):

1. expressiveness -- natural to domain experts 
    - prediction: they will say that Lispy DSLs with parens are too confusing for domain experts
2. performance -- producing optimal (in domain-specific ways) code
3. safety -- nebulous term, but effectively type safety along domain-specific vectors *only*
4. effort -- makes no sense to me

No where do they mention robustness, error handling, etc.  This is a huge problem for internal DSLs at the moment.  Adding
any kind of error reporting mucks up the implementation making it a huge mess.  The problem is that you are effectively mapping the 
semantics of a domain onto the semantics of the host language forms.  This is nice in some ways, but it is very very difficult to 
distinguish between a semantic error occurring at the host-level and one at the domain-level.  This is the downfall of 
general-purpose languages as DSL hosts.  What might work better is a special-purpose language meant for creating DSLs.

### Performance

They mention:

> Performance implies that programs in the embedded language must be amenable to extensive static and dynamic analysis, 
> optimization, and code generation, just as programs in a stand-alone implementation would be.

But that phrasing is limiting.  That's not to say that it's not powerful, just limiting.  The term static-analysis is key.

Interesting...

> Optimizing matrix operations is one of the classic examples of the use of C++ expression templates [48, 49] and is 
> used by many systems such as Blitz++ [50], A++ [35, 36], and others. 

A nice use-case for something like Clojail:

> On the other hand, language embeddings in Lisp can be too seamless in that they do not distinguish between 
> the embedded DSL and the hosting framework.

OK.  So I was wrong.  They never mentioned parentheses.  I'm too cynical :p

### L@@k

- J. Dean and S. Ghemawat. Mapreduce: Simplified data processing on large clusters. In OSDI, pages 137–150, 2004
- C. Elliott, S. Finne, and O. De Moor. Compiling embedded languages. Journal of Functional Programming, 13(03):455– 481, 2003.
- S. Gorlatch. Send-receive considered harmful: Myths and realities of message passing. ACM Trans. Program. Lang. Syst., 26(1):47–56, 2004.
- P. Hudak. Modular domain specific languages and tools. In Software Reuse, 1998. Proceedings. Fifth International Conference on, pages 134–142, 1998.
- G. L. S. Jr. Parallel programming and parallel abstractions in fortress. In IEEE PACT, page 157. IEEE Computer Society, 2005.
- K. Kennedy, B. Broom, A. Chauhan, R. Fowler, J. Garvin, C. Koelbel, C. McCosh, and J. Mellor-Crummey. Telescoping languages: A system for automatic generation of domain languages. Proceedings of the IEEE, 93(3):387–408, 2005. This provides a current overview of the entire Telescoping Languages Project.
- D. Leijen and E. Meijer.	Domain specific embedded compilers. In DSL: Proceedings of the 2 nd conference on Domain-specific languages: Austin, Texas, United States. Association for Computing Machinery, Inc, One Astor Plaza, 1515 Broadway, New York, NY, 10036-5701, USA„ 1999.
- T. Sheard and S. Jones. Template meta-programming for Haskell. ACM SIGPLAN Notices, 37(12):60–75, 2002.
- *A. van Deursen, P. Klint, and J. Visser. Domain-specific languages: an annotated bibliography.	SIGPLAN Not., 35(6):26–36, 2000.*
- T. Veldhuizen. Expression templates, C++ gems, 1996.


*Determinism in Partially Ordered Production Systems by Hellerstein and Hsu*
----------------------------------------------------------------------------

*implied-inference-ordered systems (iio)*

### L@@K

- *Towards a theory of declarative knowledge* by Apt, Blair and Walker
- *Programming expert systems in OPS5* by Brownston
- *Production system conflict resolution strategies* by Forgy
- *Pattern-directed inference systems* by Waterman and Hayes-Roth


2011.01.07
==========

*Implementation of a “Lisp comprehension” macro by Lapalme*
-----------------------------------------------------------

Origins (an "of course they were" moment)

> List com- prehensions have been introduced by David Turner in KRC, where they were 
> called ZF-expressions

Miranda's LC spec:

    [ <expression> | <qualifier1> , . . . , <qualifiern> ]

### L@@K

- S. L. Peyton-Jones. The Implementation of Functional Languages. Prentice-Hall, 1987.
- Research Software Limited. Miranda System Manual. 1987.
- D. A. Turner. Functional Programming and Its Applications, chapter Re- cursion Equations as a Programming Language, pages 1–28. Cambridge University Press, 1982.
- D. A. Turner. SASL language manual. UKC computing lab. report, The University of Kent at Canterbury, nov 1983.

Less concise Lisp comprehension:

<pre><code>
(defmacro bind-map (shape effect list)
  (let ((item (gensym)))
    `(loop for ,item in ,list 
	collecting
	  (destructuring-bind ,shape ,item
	    (funcall (lambda () ,effect))))))


;;; when you have stupid builtins like PAIRLIS that return
;;; results of a certain shape (i.e. and association list instead
;;; of a proper list, you can use bind-map to get the shape you want)
;
;(bind-map (num . string) 
;	  (list string num) 
;	  (pairlis (list 1 2 3) 
;		   (list "one" "two" "three")))
;
;=> (("three" 3) ("two" 2) ("one" 1))
;
;(bind-map (first second third)
;	  second
;	  (list (list 1 2 3)
;		(list 4 5 6)
;		(list 7 8 9)))
;
;=> (2 5 8)
;	  
;(bind-map (operator (type comment))
;	  comment
;	  '((+ (:binop "addition"))
;	    (- (:binop "subtraction"))
;	    (- (:unop  "negation"))))
;
;=> ("addition" "subtraction" "negation")
</code></pre>


*Contracts for Higher-Order Functions by Findler and Leeleisen*
---------------------------------------------------------------

Bigloo Scheme uses contracts

> With one exception, higher-order languages have mostly ignored assertion-style contracts. 
> The exception is Bigloo Scheme
> ...
> These constraints are used to generate more efficient code when the compiler can prove 
> they are correct and are turned into runtime checks when the compiler cannot prove 
> them correct.

*Think how this might be accomplished in Clojure*

The **gist** of contracts

> If x is not in the proper range, f’s caller is blamed for a contractual violation. 
> Symmetrically, if f’s result is not in the proper range, the blame falls on f itself.

HOFs

> higher-order functions complicate blame assignment.

Using [Trammel](https://github.com/fogus/trammel)

    (defconstrainedfn sqrt
      [x] [(>= x 0) => (>= % 0)]
      (Math/sqrt x))

    (defn bigger-than-zero? [n] (>= n 0))
    
    (defconstrainedfn sqrt
      [x] [bigger-than-zero? => bigger-than-zero?]
      (Math/sqrt x))

> The contract on sqrt can be strengthened by relating sqrt’s result to its argument.

Which can be done easily in Trammel:

    (defconstrainedfn sqrt
      [x] [bigger-than-zero? => bigger-than-zero? (<= (Math/abs (- x (* % %))) 0.01)]
      (Math/sqrt x))

Types vs. Contracts

> Although contracts can guarantee stronger properties than types about program execution, their 
> guarantees hold only for particular program executions. In contrast, the type checker’s weaker 
> guarantees hold for all program executions.

*TODO: Is the `wrap` function useable in Trammel?*

Module boundaries

> Contracts are most effective at module boundaries, where they serve the programmer by improving the 
> opportunities for modular rea- soning. That is, with well-written contracts, a programmer can study 
> a single module in isolation when adding functionality or fixing defects.

### L@@K

- Felleisen, M., R. B. Findler, M. Flatt and S. Krishnamurthi. How to Design Programs. MIT Press, 2001.
- Flatt, M. Composable and compilable macros: You want it when? In Proceedings of ACM SIGPLAN International Con- ference on Functional Programming, 2002.
- Leroy, X. The Objective Caml system, Documentation and User’s guide, 1997.
- Luckham, D. Programming with specifications. Texts and Monographs in Computer Science, 1990.
- Rosenblum, D. S. A practical approach to programming with assertions. IEEE Transactions on Software Engineering, 21(1):19–31, Janurary 1995.


2011.01.06
==========

*Flavors : A non-hierarchical approach to object-oriented programming by Cannon*
---------------------------------------------------------

A *Flavor* is analogous to a class.

> combining specific implementations of different protocols under control of meta-protocols

Open system

> Flavors provides more flexibility than other oop systems...
> ... conventions become more important


*Datalog: Deductive database programming by Jay McCarthy*
---------------------------------------------------------

<http://planet.plt-scheme.org/package-source/jaymccarthy/datalog.plt>

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
