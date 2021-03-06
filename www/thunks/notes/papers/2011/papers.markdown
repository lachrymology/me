---
title: Papers read in 2011
filemdate: 2011.02.25
---

{menu: {min_levels: 3}}

2011.03.10
==========

*Recursive Interfaces for Reactive Objects by Travers*
------------------------------------------------------

Talks briefly about Kay's Vivarium Project... **TODO**: find out more

### Frames

<blockquote>
Framer provides a single structure, the frame, out of which more complicated structures are built. A frame has a name and a location within a structure that is similar to a hierarchical file system. All frames except the root have a container or home frame and may also have contained frames or annotations. Frames also have an optional value (which may be any Lisp object, including another frame) and an optional prototype (which must be a frame).
</blockquote>

### Prototypes

> The advantages of prototype-based programming are simplicity and concreteness. It eliminates a whole set of objects (class 
> descriptors) from the environment, and simplifies the specification of inherited properties. In a sense it is specification by 
> example.

### GEB

> There are some indications that mental representation of categories makes use of prototypes

### Vs Self

<blockquote>
Self [14] is currently the prototypical prototype-based object-oriented programming system. LiveWorld's (really Framer's) major contribution over and above what Self provides is in making slots be first-class objects and thus enabling recursive annotation and containment.
</blockquote>

### L@@K

- Chang, B.-W. and Ungar, D. Animation: From Cartoons to the User Interface. In Proceedings of UIST `93. (November 3-5, Atlanta, Georgia), 1993, pp. 45-56.

- Kay, A. Vivarium Papers. Unpublished essays, 1990.

- Lieberman, H. Using Prototypical Objects to Implement Shared Behavior in Object Oriented Systems. In Proceedings of First ACM Conference on Object Oriented Programming Systems, Languages & Application., 1986.


*The Jini Architecture by Jim Waldo*
------------------------------------

> Jini allows anything with a processor, some memory, and a network connection 
> to offer services to other entities on the network or to use the services that are so offered.

Java...  (good quote for JoC 2nd ed.)

> Java’s most basic property is that it turns an otherwise heterogeneous network of 
> computing entities into a homogeneous collection of Java virtual machines.

### L@@k

- Dorward, S., Pike, R., Presotto, D., Ritchie, D., Trickey, H., and Win- terbottom, P. The Inferno Operating System. Tech. Rep., Bell Laboratories


*Modernizing Common Lisp: Recommended Extensions by mallery, Shrobe, Laddaga, etc.*
-----------------------------------------------------------------------------------

Wow!  Incredibly prescient!

- Transactional Memory
- Java interop
- Parallelism
- Mobile Lisp
- VM
- Event Model


*High-order Logic Programming in Prolog by Naish*
-------------------------------------------------

Describes something called "skeletons and techniques".  Not sure what it means based on this paper.

### L@@k

- *HiLog: A foundation for higher-order logic programming* by Chen, Kifer, and Warren
- *Mercury: an efficient purely declarative logic programming language* by Zoltan, Henderson, and Conway
- *Applying techniques to skeletons* by Sterling and Kirschenbaum
- *Higher-order extensions to Prolog: are they needed?* by Warren


*The Tuple Space: An Old Solution to a New Problem? by Friday and Wade*
-----------------------------------------------------------------------

### L@@k

- N. Davies, S. Wade, A. Friday and G. S. Blair, "Limbo: A Tuple Space Based Platform for Adaptive Mobile Applications", Proceedings of the International Conference on Open Distributed Processing/Distributed Platforms (ICODP/ICDP '97), Toronto, Canada, 27-30 May 1997. Internal report number MPG-97-02.

- D. Gelernter, "Generative Communication in Linda.", ACM Transactions on Programming Langauges and Systems, Vol. 7 No. 1, Pages 80-112. January 1985.

- D. Gelernter, N. Carriero, S. Chandran and S. Chang, "Parallel Programming in Linda", Proceedings of the International Conference on Parallel Processing, pp 255-263, August 1985.


2011.03.02
==========

*Factor: A Dynamic Stack-based Programming Language by Pestov and Ehrenberg*
----------------------------------------------------------------------------

CLOS-based generic function (as opposed to message passing) object system.

**TODO**: Not sure that I understand row-polymorphism.

### L@@k

- Daniel G. Bobrow and Gregor Kiczales. The Common Lisp Object System metaobject kernel: a status report. In LFP ’88: Proceedings of the 1988 ACM conference on LISP and func- tional programming, pages 309–315, New York, NY, USA, 1988. ACM.
- Robert Cartwright and Mike Fagan. Soft typing. In PLDI ’91: Proceedings of the ACM SIGPLAN 1991 conference on Pro- gramming language design and implementation, pages 278– 292, New York, NY, USA, 1991. ACM.
- Daniel Ehrenberg. Closure elimination as constant propagation. Programming Language Design and Implementation, Student Research Contest, 2010.
- Michael Ernst, Craig Kaplan, and Craig Chambers. Predicate Dispatching: A Unified Theory of Dispatch. In ECCOP ’98: Proceedings of the 12th European Conference on Object- Oriented Programming, pages 186–211, London, UK, 1998. Springer-Verlag.
- David Gudeman. Representing Type Information in Dynamically Typed Languages, 1993.
- Xavier Leroy.	The Objective Caml system – Documentation and user’s manual. http://caml.inria.fr/pub/docs/manual-ocaml/.
- Manfred von Thun. Rationale for Joy, a functional language, 2000.


2011.02.23
==========

*The Development of the C Language by Dennis Ritchie*
-----------------------------------------------------

BCPL -> B -> C

The original UNIX was in assembler, developed on another platform.  Thompson did not move to the PDP-7 until he had created the foloowing:

- kernel
- editor
- assembler
- simple shell
- utilities: rm, cat, cp

Doug McIlroy created the first HL-language TMG -- a lang for writing compilers

**Did these guys get paid to just sit around and do awesome stuff?!**

### L@@k

- R. H. Canaday and D. M. Ritchie, `Bell Laboratories BCPL,' AT&T Bell Laboratories internal memorandum, May, 1969.
- S. C. Johnson and B. W. Kernighan, `The Programming Language B,' Comp. Sci. Tech. Report #8, AT&T Bell Laboratories (January 1973).
- S. C. Johnson, `A Portable Compiler: Theory and Practice,' Proc. 5th ACM POPL Symposium (January 1978).
- R. M. McClure, `TMG—A Syntax Directed Compiler,' Proc. 20th ACM National Conf. (1965), pp. 262-274.
- G. Nelson, Systems Programming with Modula-3, Prentice-Hall: Englewood Cliffs, NJ, 1991.
- M. Richards, `The BCPL Reference Manual,' MIT Project MAC Memorandum M-352, July 1967.
- M. Richards and C. Whitbey-Strevens, BCPL: The Language and its Compiler, Cambridge Univ. Press: Cambridge, 1979.
- R. Sethi, `Uniform syntax for type expressions and declarators,' Softw. Prac. and Exp. 11 (6), June 1981, pp. 623-628.
- J. E. Stoy and C. Strachey, `OS6—An experimental operating system for a small computer. Part I: General principles and structure,' Comp J. 15, (Aug. 1972), pp. 117-124
- C* Programming Guide, Thinking Machines Corp.: Cambridge Mass., 1990.
- K. Thompson, `Bon—an Interactive Language,' undated AT&T Bell Laboratories internal memorandum (ca. 1969).


2011.02.21
==========

*Aspect-Oriented System Structure by Gregor Kiczales*
-----------------------------------------------------

They say they wish to "introduce", but then they use terms as if I know them.  :-(

Talks about [AspectC](http://www.cs.ubc.ca/labs/spl/aspects/aspectc.html)


2011.02.17
==========

*Overloading vs. Object Technology by Bertrand Meyer*
------------------------------------------------------

The gist:

> Different things should have different names

Shows a nice rename feature of Eiffel.

**LOL**

> the first case of defining a language’s semantics through compiler warnings. Semantics should be defined very 
> precisely, since it conditions the validity of programs and the effect of valid programs


*Toward More Expressive Contracts by Bertrand Meyer*
----------------------------------------------------

Using functions to gain more expressiveness in conditions.


2011.02.15
==========

*Practice to Perfect: The Quality First Model by Bertrand Meyer*
----------------------------------------------------------------

> Quality First is bottom-up (it starts with perfecting a small system then adds functionality)

Motto:

> Build it so you can trust it. Then don’t trust it

On comments, etc.

> Like everyone else I am occasionally tempted to cut corners and postpone writing header comments, 
> indexing clauses, and the like. But I censure myself because I know it means slower progress in the
> end.

No love for dynamic langs:

> Why would anyone use an untyped or dynamically typed language?  The argument “we’ll develop faster that
> way” makes no sense to me, either theoretically or practically

Demo

> always have a working system


### L@@k

- Kim Walden “Reversibility in Software Engineering” Sept. 1996
- Tom Van Vleck's homepage <http://www.multicians.org/thvv/>

2011.02.14
==========

*Design by Contract: The Lessons of Ariane by Bertrand Meyer*
-------------------------------------------------------------

> The only realiztic test is launch

One of the principles of design by contract, as earlier columns have said, is that any software element that has such a fundamental constraint should state it explicitly, as part of a mechanism present in the language.


*Reality: A cousin twice removed by Bertrand Meyer*
---------------------------------------------------

Modelling the "real world" is a myth!

> reality is in the eye of the beholder

> software is at best only a model of a model of some part of reality

Allegory of the cave.


*Why your next project should use Eiffel by Bertrand Meyer*
-----------------------------------------------------------

Too bad no one did.  My life as an OOP developer would have been much nicer.

- static typing
- assertions
- exceptions
- gc
- inheritance (even multiple)
- late binding

### L@@k

- "short-form" classes?  How to extend this to Clojure functions and types?
- Eiffel's runtime engine (few thousand lines of C)
- 

*The many faces of inheritance by Bertrand Meyer*
-------------------------------------------------

### 3 bad inheritances

1. has-a inheritance
2. taxomania - useless intermediate nodes
3. convenience inheritance - I need a method now

### 12 types of inheritance

1. subtype inheritance - base describes an incomplete specification, deferred until extension
2. view inheritance - A and B are the same abstraction, but viewed from different angles (?)
3. restriction inheritance - B is a more restrictive class of A (e.g. Ellipse <- Circle). constraints are deferred until extension
4. extension inheritance - B introduces feature not found in A
5. functional variation inheritance - B redefines some features of A (breaks Liskov)
6. type variation inheritance - B redefines some features of A at the signature level only
7. uneffecting inheritance - B redefines some of A's concretions to be abstract (i.e. deferrals)
8. reification inheritance - B extends A but not fully.  Leaves room for more deferral
9. structure inheritance - A defines a property (e.g. Comparable), and B represents a concretion possessing that property
10. implementation inheritance - no deferrals in either A nor B
11. constant inheritance - A provides a set of logically related constants
12. machine inheritance - A provides a set of logically related methods


*The reusability challenge by Bertrand Meyer*
---------------------------------------------

*What is  an  object-oriented environment? by Bertrand Meyer* 
-------------------------------------------------------------

Describes EiffelBench

LoL

> In environments supporting the most popular analysis methods, the little clouds and bubbles which 
> seem to be the main selling points of these tools accurately reflect the vagueness surrounding the 
> methods' theoretical foundations and practical usefulness.


*Design by contract: building bug-free O-O software by Bertrand Meyer*
----------------------------------------------------------------------

> When quality is pursued, productivity follows

The law of excluded miracles:

> it is amazing to see how far just stating what a module should do goes towards helping to ensure that it does it.


*Introduction to Agda by Daniel Peebles*
----------------------------------------

Cool fixity syntax decl.  How to expand on this?

    def _! <- |n|
      factorial n.
    
    5!
    -- 120
    
    def if_then_else_ <- |condition, then-part, else-part|
      if condition:
        ,then-part
        ,else-part
    
    if true then
      42
      36
    
    -- 42

Lambda op syntax

Sequential source code with optional code blocks.

### GADTs (Generalized Algebraics Data Types)

### Dependent Types

- Types depend on values
- Phantom types
- Type of second component determined by value of first!


2011.02.08
==========

*An Axiomatic Basis for Computer Programming by Hoare*
------------------------------------------------------

> The first requirement in valid reasoning about a pro- gram is to know the properties of the elementary operations which it invokes

whoa!

> he proven result of the first part of a program is identical with the precondition under which the second part of the program 
> produces its intended result, then the whole program will produce the intended result, provided that the precondition of the 
> first part is satisfied.



2011.02.07
==========

*Developing DSLs using combinators. A design pattern by Barrientos and Lopez*
-----------------------------------------------------------------------------

Explore further how currying reduces parens.

*Computer architecture for functional programming*
--------------------------------------------------

A list of fun languages to explore:

- FP          (Backus)
- SASL        (27)
- Id          (2)
- VAL         (1)
- Prolog      (7)
- Lucid 3     (3)

### Categories of languages

- imperative
- single assignment (fp)
- applicative (fp)
- logic
- actor (oop)

### L@@k

1. *VAL - a value oriented algorithmic language* by Ackerman, 1978
2. *Asynchronous programming language and computing machine* by Arvind, 1978
3. *LUCID: a non-procedural language with iteration* by Ashcroft and Wadge, 1977
4. *Reduction languages for reduction machines* by 1975
5. *A view of dataflow* by Gostelow and Thomas, 1979
6. *The Smalltalk-76 programming system design and implementation* by Ingalls
7. *Algortihms = Logic + Control* by Kowalski, 1979
8. *Principal components of a data flow computer* by Treleaven, 1980
9. *Recursive machines* by Wilner, 1980


*Interpreters for functional programming by David Wise*
-------------------------------------------------------

Describes something called "ribcage environments".  

I really need to re-read this for wzrdzkül.

### L@@k

- *Recursive programming techniques* by Burge, 1975  (re-read)
- *Managing reentrant structures using reference counts* by Bobrow, 1980
- *A method for overlapping and erasure of lists* by Collins, 1960
- *CONS should not evaluate its arguments* by Friedman, 1976
- *Functional combination* by Friedman, 1978
- *An interpretive model for a language based on suspended construction* by Johnson, 1977
- *A correspondence between ALGOL 60 and Church's lambda notation* by Landin, 1965
- *Definitional interpreters for higher-order programming languages* by Reynolds, 1972
- *Correct and optimal implementation of recursion in a simple programming language* by Vuillemin, 1974


2011.02.04
==========

*Program Transformation by Darlignton* 
--------------------------------------

Functional programs can often be transformed via algebra rather than some ad-hoc complex way.

Every paper cites Backus!

### L@@k

- *Proving theorems about Lisp functions* by Boyer and Moore
- *Design considerations for a fp language* by Burstall
- *The control facilities of IC-Prolog* by Clark and McCabe
- *Predicate Logic: a calculus for the derivation of programs* by Clark and Sickel
- *Logic as a computer language* by Kowalski
- *Using annotations to make recursion equations behave* by Shwarz



2011.01.28
==========

*Purely functional operating systems by Peter Henderson*
--------------------------------------------------------

Awesome!


*Real programming in functional languages by James Morris*
----------------------------------------------------------

Whoa!  In the first paragraph the author manages to offend Backus, McCarthy, Landis, etc. by calling them meta-programmers!

### L@@k

- *Recursive programming techniques* by Burge
- *CONS should not evaluate its arguments* by Friedman and Wise
- *The Lisp70 pattern matching system* by Tesler
- *TUNIS: A UNIX like operating system written in EUCLID* by Holt


*Notes on using types and type abstractions in fp by Guttag*
------------------------------------------------------------

TODO: Explore further the idea of *axioms*.

### L@@k

- *The Simula 67 common base language* by Dahl, Nygaard, Myhrhuag
- *The next 700 programming languages* by Landin


2011.01.26
==========

*Generalized Combinators in Functional Languages and Their Applications by Jozef de Man* 
----------------------------------------------------------------------------------------

Uses a variant of SASL

`foldr` and `foldl` have the same results when the function used is both associative and commutative.

    (defn iota [t nxt stop y]
      (take-while stop (iterate #(t (nxt %)) y)))
    
    (iota identity inc #(< % 10) 1)
    
    (def upto (fn [end start]
                (iota identity inc #(< % end) start)))
    
    (def downto (fn [end start]
                  (iota identity dec #(> % end) start)))
    
    (upto 10 1)
    (downto 10 20)
    
    (defn to [start end]
      (if (<= start end)
        (upto end start)
        (downto end start)))
    
    (to 10 20)
    (to 20 10)
    (to 5 -5)

Fun fun fun.

### L@@k

- Backus, J.W. "Functional Level Programs as Mathematical Objects" in Proc. ACM	Conf. on Functional Programming Languages and Computer Architecture, Portsmouth, NH (Oct 1981).
- Burstall, R.M., Darlington, J.A. "A transformation system for	developing recursive programs" Journal of the ACM 24,l (Jan 1977).
- Burstall, R.M., HOPE: An experimental applicative language
- Turner, David, A New Implementation Technique for Applicative Languages
- Wadler, Applicative Style programming, Program transformation, and List Operators


*Design by Contract: A Simple Technique for Improving the Quality of Software by Bolstad*
-----------------------------------------------------------------------------------------

> In Defensive Programming every error and/or failure should be caught and handled by the software. There 
> is no distinction between an error (a correctness problem) and a failure (a robustness problem). In 
> DbC, there is a clear distinction between these two concepts. It is the responsibility of the 
> programmer to fix all correctness errors.

### Hoare Triples

    {P}        instructions {Q}
    requires                ensures
    :pre                    :post

Summarized as:

> Any execution of A started in a state satisfying P will terminate in a state satisfying Q.

### L@@k

- Hoare, C.A.R., “An Axiomatic Basis for Computer Programming”, in Communications of the ACM, vol. 12, no. 10, October 1969.


*An inference engine for function free logic programs by Stefan Bottcher*
-------------------------------------------------------------------------

Describes a language PROTOS-L that:

> PROTOS-L is similar to DATALOG embedded in a typed logic programming language.

Provides:

- access to relational databases
- a module concept (based on MODULA-2, TEL, and DBPL)
- a type concept including subtypes and polymorphisim
- supports the programming of deductive databases

### 2 modes of operation

1. Database body - provides set-oriented processing
2. Program body - via bactracking

### L22k

- [Bancilhon and Ramakrishnan, 19861 F. Bancilhon and R. Ramakrishnan. An amateur's introduction to recursive query processing. In Proceedings of the ACM SIGMOD International Conference on Management of Data, Washington D.C., 1986.
- [Bottcher, 1990bI S. Bottcher.	Integrating a deductive database system with a Warren Abstract Machine. In N. Cercone, F. Gardin, and G. Valle, editors, PTOC.Computational Intelligence 111 - The International Conference on Compuiational Intelligence 90, Milan, Italy, 1990. (to appear).
- [Hulin, 19891 G. Kulin. Parallel processing of recursive queries in distributed architectures. In Proceedings of ihe lbhInternational Conference on Very Large Data Bases, Amsterdam, 1989.
- [Warren, 19831 D.	Warren.	An Abstract PROLOG Instruction Set. Technical Report 309, SRI, 1983.

*Datalog vs. First-order Logic by Ajtai and Gurevich*
-----------------------------------------------------

Datalog v Prolog in a nutshell

> (Pure) datalog may be seen as pure prolog without function symbols (of positive arity).

2011.01.20
==========

*Curry: A truly functional-logic language by Hanus and Kuchen*
--------------------------------------------------------------

### Problems of current logic languages

- nondeterminism
- non-declarative cuts
- non-declarative i/o



*Evaluating Haskell in Haskell by Matthew Naylor*
-------------------------------------------------

Combinator reduction for Haskell

### L@@k

- Jan Martin Jansen, Pieter Koopman, and Rinus Plasmeijer. Efficient interpretation by transforming data types and patterns to functions. In Trends in Functional Programming, volume 7. Intellect (2007)
- Lennart Augustsson. Small – a small interactive functional system. Technical Re- port 28, Programming Methodology Group, University of Goteborg and Chalmers University of Technology (1986).
- M. Hanus, H. Kuchen, and J.J. Moreno-Navarro. Curry: A truly functional-logic language. In ILPS’95 Post Conference Workshop on Declarative Languages for the Future. Portland State University and ALP, Melbourne University (1995).



*SKIM - The S, K, I Reduction Machine by Clarke, Gladstone, MacLean, and Norman*
--------------------------------------------------------------------------------

Combinators used as an intermediate form for applicative languages and the machines that run them.

Describes the Small language (for symbolic algebra)
  - first-class function
  - call-by-need
  - robust error handling

> Experience with the initial interpretive implementation of Small rapidly convinced us that any loss in 
> expressive power that may result from removing imperative constructs from a language is more than 
> balanced by the convenience of having normal order evaluation and higher order functions.

### Combinators

> In its most primitive form combinatory logic is built up using just the two symbols S and K, which 
> represent functions satisfying 

    K x y : x
    S f g x = f x (g x)

and `I` would be:

    I = S K K

### L@@k

- Turner, D. A. "A new implementation technique for applicative languages" Software Practise & Experience, 1979
- Norman, A. C. and Moore, P. M. A. ',The design of a vector-based algebra system"	Proc. EUROSAM 79, 1979 (Springer Lecture Notes in Computer Science 71, ed: E. Ng)
- Fitch, J.P. and Norman, A. C. "Implementing LISP in a high-level language" Software Practise and Experience, 1977



*LISP, Programming and Implementation by Sussman*
-------------------------------------------------

Implements an `eval` and `apply`.  Looks a lot like pg's impl.

### L@@k

- *Viewing control structures as patterns of passing messages* by Hewitt, 1977
- *The History of Lisp* by McCarthy
- *The dream of a lifetime: a lazy scoping mechanism* by Sussman and Steele, 1979
- *Continuation-based program transformation strategies* by Mitchell Wand, 1977



*Recursion equations as a programming language by D. Turner*
------------------------------------------------------------

This paper is a good basis for a presentation.

This paper desccribes the Kent Recursive Calculator (KRC).

KRC is preceeded by SASL.

    [1..]

is the way to express all natural numbers... is `inc` the default operation for the transition?

### L@@k

- *Can programming be liberated from the Von Neumann style?* by John Backus, 1978
- *SKIM - S, K, I reduction machine* by Clarke, Gladstone, Norman, and Maclean, 1980
- *The varieties of dataflow computers* by Dennis, 1979
- *Assigning meanings to programs* by Floyd, 1967
- *A lazy evaluator* by Henderson and Morris, 1976
- *A guide to CSP* by Kuo, Linck, and Saadat, 1978

2011.01.18
==========

*How to tell truths that might hurt? by Edsger W. Dijkstra*
-----------------------------------------------------------

Computing science seems to suffer from the unwillingness to tell harsh truths.

> With respect to COBOL you can really do only one of two things:
> fight the disease or pretend it does not exist.

*good quote/ref for JoC*


2011.01.14
==========

*Dynamically Scoped Functions as the Essence of AOP by Costanza*
----------------------------------------------------------------

### Benefits

> dynamically scoped variables turn out to be very useful when there is a need to influence the behavior of 
> parts of a program with- out having to clutter the parameter lists of the functions called directly and/or 
> indirectly.

*which is Stallman's thinking also*

### symbol macros emulating global lexicals

> In those rare cases in which a lexically scoped global variable is actually needed, because it is important 
> to be able to rebind it lexically, define-symbol-macro can be used to emulate it.

**TODO: understand this more** refs (1,18)

> Common Lisp provides means to declare dynamically scoped local variables.

**TODO: So does Clojure... possible blog post. L@@k when it is useful** ref (3)

### AOP impl

- The decision for lexical or dynamic scope is to be made alongside the local definition of a function. (a la `(binding ...)`)

      (defn f [x] (println x))
      (defn g [x] (f x))
      
      (let [h (fn [x] (g x))]
        (binding [f (fn [x] (print (inc x)))]
          (h 5)))
      ; 6

    + **TODO: implement dflet in Clojure**

- Dynamically scoped function definitions is not enough. We also provide a way to refer to the previous definition of a function by way of an implicit local call-next-function definition.
    + something like:

          (let [h (fn [x] (g x))]
            (dflet [(f [x] 
                      (call-next-fn (+ 1 x)))]
              (h 5)))

    + This let's the canonical AOP example occur

          (defn f [x] (println x))
          
          (dflet [(f [x] (println "entering f")
                         (call-next-fn)
                         (println "leaving  f"))]
            (f 5))

### Nutshell statement

- A dflet captures specific join points in the control flow of a program, and its definitions cross-cut the program at each invocation of the original functions.
- A dflet has dynamic extent: As soon as the control flow enters a dflet, the new function definitions are activated and on return, they are deactivated again.
- So with dflet, there is no need to add constructs for statically reasoning about the control flow of a program

### Translation

    (defun f (x) (* x x))

translates into

    (defvar *f* (lambda (x) (* x x))) 
    
    (defun f (&rest args) (apply *f* args))

Using a new `defdynfun`

### multidflet

    (defmacro multidflet ((functions &body def) &body body)
      ‘(dflet ,(mapcar 
                (lambda (function)
                  ‘(,function (&rest args) ,@def)) 
                functions)
      ,@body))

### L@@k

- J. Baker and W. Hsieh. Runtime Aspect Weaving through Metaprogramming. In: AOSD 2002 - Pro- ceedings. ACM Press, 2002.
- R. Filman and D. Friedman.	Aspect-Oriented Programming is Quantification and Obliviousness. Workshop on Advanced Separation of Concerns, OOPSLA 2000, Minneapolis, USA.
- T. Bradshaw. Maintaining dynamic state, 2001. http://www.tfeb.org/lisp/hax.html#DYNAMIC- STATE
- D. Hanson and T. Proebsting. Dynamic Variables. In: PLDI 2001 - Proceedings. ACM Press, 2001.
- R. Lammel. A Semantical Approach to Method Call Interception. In: AOSD 2002 - Proceedings. ACM Press, 2002.
- K. Pitman (ed.). Common Lisp HyperSpec, 2001. http://www.lispworks.com/reference/HyperSpec/
- D. Tucker and S. Krishnamurthi. Pointcuts and Ad- vice in Higher-Order Languages. In: AOSD 2003 - Proceedings. ACM Press, 2003.
- W. De Meuter. Monads as a theoretical foundation for AOP. In: International Workshop on Aspect- Oriented Programming at ECOOP, 1997.
- J. Lewis, J. Launchbury, E. Meijer, M. Shields. Implicit Parameters: Dynamic Scoping with Static Types. In: POPL 2000 - Proceedings. ACM Press, 2000.



*Using Domain Specific Language For Modeling And Simulation: Scalation As A Case Study By Miller, Han, and Hybinette*
---------------------------------------------------------------------------------------------------------------------

Wow... has a lot of citations.

> This paper considers two issues in the development of simulations: (i) narrowing the gap between model and program 
> and (ii) using an embedded Domain Specific Language (DSL) rather than a General Purpose Language (GPL) or Simulation 
> Programming Language (SPL).

### Scala is good for DSLs because...

- Operator overloading
- Type inference
- Type alias
- First-class functions
- Functional programming
- Default args
- Parser combinator lib

No mention of implicits?

### L@@k

- Dubochet, G. 2006. On Embedding Domain-specific Languages with User-friendly Syntax. In Proceedings of the 1st Workshop on Domain-Specific Program Development, 19–22. Nantes, France.
- Miller, J. A., G. T. Baramidze, A. P. Sheth, and P. A. Fishwick. 2004. Investigating Ontologies for Simulation Modeling. In ANSS ’04: Proceedings of the 37th Annual Symposium on Simulation, 55–71. Washington, DC, USA: IEEE, Inc. Available via: <http://www.cs.uga.edu/~jam/DeMO>.
- Nance, R. E. 1996. A History of Discrete Event Simulation Programming Languages. In History of Programming Languages—II, 369–427. New York, NY, USA: ACM.
- Smith, J. B. 2006. Practical OCaml. Berkeley, California, USA: Apress.
- 

*A Java Fork/Join Framework by Doug Lea*
----------------------------------------

*3rd or 4th reading -- classic*

Problems hosting fj on Java threads:

> he java.lang.Thread class (as well as POSIX pthreads, upon which Java threads are often based) are 
> suboptimal vehicles for supporting fork/join programs

- fj tasks never need to block except at aggregate levels (i.e. waiting on sub-tasks)
    + so thread book-keeping is wasteful
- fj tasks are often very small
    + so creating and destroying threads can be prohibitively costly by comparison

Based on the design of Cilk.

### Dequeues

Dequeues reduce contention when stealing.

> Because the deque array is accessed by multiple threads, sometimes without full synchronization, yet 
> individual Java array elements cannot be declared as volatile, each array element is actually a fixed 
> reference to a little forwarding object maintaining a single volatile reference.

### GC

> In many ways, modern GC facilities are perfect matches to fork/join frameworks: These programs can generate enormous 
> numbers of tasks, nearly all of which quickly turn into garbage after they are executed.

Generational GC meshes well with parallelism.

### Locality

Worker threads that consume the tasks they create are more efficient than stealing.  Some tasks are more suited to locality than others.  

### L@@k

- Frigo, Matteo, Charles Leiserson, and Keith Randall. The Implementation of the Cilk−5 Multithreaded Language. In Proceedings of 1998 ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI), 1998.


2011.01.13
==========

*How to Make Lisp More Special by Costanza*
-------------------------------------------

Kinda boring

### L@@K

- Henry Baker. Shallow Binding in Lisp 1.5. Communications of the ACM 21, 7	(July	1978),	565-569.	Available: http://home.pipeline.com/∼hbaker1/Shallow- Binding.html
- <ftp://ftp.parc.xerox.com/pub/pcl/archive>
- Pascal Costanza. Dynamically Scoped Functions as the Essence of AOP. ECOOP 2003 Workshop on Object-Oriented Language En- gineering for the Post-Java Era, Darm- stadt, Germany, July 22, 2003. ACM Sig- plan Notices 38, 8 (August 2003). Available: http://www.pascalcostanza.de/dynfun.pdf


*Aspects of PROLOG History: Logic Programming and Professional Dynamics by Rouchy*
----------------------------------------------------------------------------------

### Parallel

- *and-parallelism* - when more than one sub-goal solves a query, some sub-goals are executed in parallel
    + list processing
- *or-parallelism*  - when more than one clause solves a query, some cluases are executed in parallel
    + backtracking expert systems
       - language parsing
       - optimisation problems
       - deductive dbs
- *unification parallelism*
    Unify(Arg1, Arg2) = 
      If (Arg1 is a complex term f(t1, …, tn) and 
          Arg2 is a complex term g(s1, …, sm)) 
      then 
        If (f is equal to g and n is eqaul to m) then 
          unify (t1, s1), 
          unify (t2, s2), 
          …, 
          unifiy (tn, sn) 
        Else 
          Fail

### 5th gen

- GHCs
- Parallel Inference Machine (PIM)
- Kernel0 (KL0) language was Prolog
- Kernel1 (KL1) language was (to be?) based on GHCs

### Prolog embedded in Lisp

> They (Mellish and Hardy 1982) critically advance that providing PROLOG with convenient
> connection to LISP is a complete solution.

### L@@k

- K. Clark and Gregory (1986) ‘PARLOG: A parallel implementation of PROLOG, ACM Trans. On Programming Languages Systems, vol. 8, no. 1: 1- 49.
- J. Chassin de Kergommeaux, Philippe Codognet (1994) ‘Parallel Logic Programming Systems’, ACM Computing Surveys, vol. 26, no. 3, September 1994: 295-336.
- A. Colmerauer (1985) ‘PROLOG in 10 figures’, Communications of the ACM, December, volume 28, number 12: 1296-1310.
- A. Colmerauer & P. Roussel (1992) ‘The Birth of PROLOG’ ACM SIGPLAN Notices, volume 28, no. 3, March 1993 and In Thomas J. Bergin and Richard G. Gibson, (eds.) (1996) History of Programming Languages, ACM Press/Addison- Wesley: 331-367.
- J. Cohen (1988) ‘A View of the Origins and Development of PROLOG’, Communications of the ACM, vol. 31, no. 1, 26-36.
- R. Ennals (1982) Beginning micro-PROLOG, Ellis Horwood and Heinemann, Chichester and London.
- R. Ennals, J. Briggs & D. Brough (1984) ‘What the naïve user wants from PROLOG’ in J.A. Campbell (ed.) Implementations of PROLOG, Ellis Horwood Limited, Chichester: 376-386.
- K. Fuchi, R. Kowalski, K. Furukawa, K. Ueda, K. Kahn, T. Chikayama, E. Tick (1993) ‘Launching New Era’, Communications of the ACM, vol. 36, no. 3, March 1993: 49-100.
- C. Hewitt and G. Agha (1988) ‘Guarded Horn clause languages: are they deductive and Logical?’ International Conference on Fifth Generation Computer Systems, Ohmsha 1988. Tokyo. Also in Artificial Intelligence at MIT, Vol. 2. MIT Press 1991.
- R. Kowalski (1988) ‘The early years of Logic Programming’, Communications of the ACM, January.
- J. McCarthy. (1958) ‘Programs with common sense’, Symposium on Mechanization of Thought Processes,	National	Physical	Laboratory, Teddington, England.
- J. A. Robinson and E. E. Silbert (1982b) ’LOGLISP:	Motivation,	Design	and Implementation’ in K. L. Clark & S-A. Tarnlund (eds.) (1982) Logic Programming, Academic Press, New York.
- E. Shapiro (Ed.) (1987) Concurrent Prolog: Collected Papers, vol. 1 & 2, MIT Press, Cambridge, MA.


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

> Optimizing matrix operations is one of the classic examples of the use of C++ expression templates (48, 49) and is 
> used by many systems such as Blitz++ (50), A++ (35, 36), and others. 

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
