---
title: Annotated Scala Levels
filemdate: 2011.02.25
---

The Annotated Scala Levels
==========================

These are notes that I've accumulated over the past 3+ years of exploring and using Scala in my free and workplace time.  This is not the entire set as I've yet to dig into my notebooks and marginalia for more.  However, the electronic versions that I've located have been twisted to fit into a pseudo-annotated version of Martin Odersky's [*Scala Levels* blog post](http://www.scala-lang.org/node/8610) with additional sections derived from [Tony Morris's extended levels](http://blog.tmorris.net/critique-of-oderskys-scala-levels/).  Regardless whether I agree with Dr. Odersky or Mr. Morris, I felt that their posts provided a nice outline for my notes.

*this is a living document and therefore subject to change.  however, it should be noted that [I no longer have a job that affords me the opportunity to explore scala][bai-scala] on a daily basis, so keep that in mind if updates prove to be infrequent.  there is a place to comment at the bottom if you would like to provide feedback.  additionally, [this page can be forked and modified via github](https://github.com/fogus/me/blob/master/www/src/thunks/scala.page) if you're so inclined.  in fact, that would be ideal!*

[bai-scala]: http://blog.fogus.me

Prerequisites 
-------------

Java 1.6 and Scala 2.8.1

Table of Contents
-----------------

{menu: {min_levels: 2}}

Level A1: Beginning application programmer
==========================================

Java-like statements and expressions
------------------------------------

I'm of the firm opinion that to fully appreciate the power of [Scala](http://scala-lang.org), one must begin their exploration holding the appropriate knowledge required to understand Josh Bloch's book [Effective Java](http://www.amazon.com/o/asin/0321356683?tag=fogus-20).  

### standard operators

In Scala every operator is actually a method call.  For example:

    1 + 2
    //=> 3

... is syntactic sugar for the following:

    (1).+(2)
    //=> 3



Scala tends to reserve operators for limited (although not always well-known) use cases:

- Mathematical operators (e.g. `+`, `-`, `*`, `/`, etc.)
- Comparision operators (e.g. `>`, `<=`, etc.)
- Logical operators (e.g. `&&`, `||`, etc.)
- Shorthand notation (e.g. `++`, `/:`, `\:`, etc.)
- DSL elements

While Scala has a rich set of operators in the core, but there is no reason why you cannot define your own.  However, having said that bear in mind that operators are not always intuitive in their meaning and designing them into your APIs should be done thoughtfully.

*TODO: Thrush*

### method calls

Method calls in Scala (can) look exactly like those in Java:

    val L = List(1,2,3,4,5)
    L.slice(1,4)
    //=> List(2,3,4)

Scala methods of zero or one argument can be called without the parentheses:

    L reverse
    //=> List(5, 4, 3, 2, 1)
    
    L take 3
    //=> List(1,2,3)

This fact forms the basis for Scala's operator notation (touch on above).  That is, Scala provides many core operators that can look unlike methods calls, providing a more intuitive and fluent flow:

    0 :: L
    //=> List(0, 1, 2, 3, 4, 5)
    
    (1).+(2).*(3)
    //=> 9
    
    (1+2) * 3
    //=> 9

Building fluent interfaces that take advantage of the 0-1 arg operator notation will later be used to build rich DSLs.

### conditionals

Conditionals in Scala look and act much like their Java counterparts:

    if (true) 
      println("tru dat") 
    else 
      println("never printed")
    
    //tru dat

However, where Scala differs (and in a huge way) is that *everything is an expression*

    val res = if (true) 42 else 9 
    res
    //=> 42

That is, the result of a Scala conditional can be used as a value.  Programming language statements are the spawn of Satan that tend to cause solutions to lose meaning as they grow.  Scala rightfully eschews statements and takes a functional, expression-oriented approach.[^unit]

[^unit]: However, Scala can "simulate" statements by returning the `Unit` type... a value with a rough equivalency to Java's `void` -- although describing it in this way is to simplify to the point of farce.  The exact [nature of `Unit`](http://james-iry.blogspot.com/2009/07/void-vs-unit.html) is more riche than this, and is tackled much more [eloquently by James Iry](http://james-iry.blogspot.com/2009/08/getting-to-bottom-of-nothing-at-all.html).

### loops

Again, loops in Scala can look very similar to those in Java:

    var i = 0;
    while (i < L.length) { 
      print(L(i))          
      i += 1
      i             
    }
                          
    // 12345

... and also in a way similar to Java's `for` statement:

    for (e <- L.reverse) {
      print(e)
    }
    
    // 54321

Additionally, Scala provides a `foreach` method that takes a function as an argument and runs it against each element in a collection:

    L.foreach(print)
    // 12345

Scala also allows anonymous functions to be passed as well:

    L.foreach(e => print(e.toString + "|"))
    // 1|2|3|4|5|

Used in these ways the looping constructs act very much like Java's looping statements (they will return `Unit` rather than nothing), however `for` is much richer than this naive usage belies, but I will go into more detail later.  

### try/catch

Once again, Scala's `try/catch` looks quite similar to Java's:

    var i = 0
    
    try { 
      i += 1
      error("fail") 
    } catch { 
      case e:Exception => i -= 10 
    }
    
    i
    //=> -9

The Scala `catch` clause uses the pattern matching syntax, but I will save the discussion of that until later.  Like most things in Scala, the `try/catch` is an expression and can be used to return values based on the results therein:

    val res = try { 
      i += 1
      error("fail") 
    } catch { 
      case e:Exception => {
        i -= 10
        42
      }
    }
    
    //=> 42

Of course, all of the types along the `try/catch` paths *must* be compatible[^any-catch]:

    val res:Symbol = try { i += 1; 'foo } catch { case e:Exception => i -= 10; i }
    
    // error: type mismatch;
    // found Int required Symbol

[^any-catch]: Although if types are not specified, then Scala will attempt to infer them, resulting in oddities:  `val res = try { i += 1; 'foo } catch { case e:Exception => i -= 10; i }` ---> `'foo:Any`

Scala basics
------------

While you can easily write Java-flavored Scala code, to do so beyond the initial explorations is folly.  I touched on this a bit in my talk [Naïveté vs. Experience](http://blog.fogus.me/2011/01/10/dancing-monkey-gibbers-on-about-scala-and-clojure/), but there is much more to say about the matter -- of which I will not do here.

*important terms: [singleton](http://en.wikipedia.org/wiki/Singleton_pattern), [lexical](http://www.gnu.org/software/guile/manual/html_node/Lexical-Scope.html)*

### class

Classes in Scala operate along the same dimensions as java classes:

     class Name(first:String, last:String)
     
     new Name("mike", "fogus")
     //=> Name@27adc5f7

However, as you can see the Scala class syntax gives you many things for free.  More information can be found in first chapter of any of the existing Scala books.

### object

The `object` keyword in Scala is used to define a singleton:

    object App {
      def main(args:Array[String]) = println("Hello Cleveland!")
    }
    
    App.main(null)
    // Hello Cleveland!

Objects can also be used as companions to classes, but I won't get into that deeply here.  Instead, read [First Steps to Scala](http://www.artima.com/scalazine/articles/steps.html) by Bill Venners if you want the beginner's guide to that technique.

### def

The `def` keyword can be used to define functions:

    def hi(msg:String) = println("Hello " + msg)
    
    hi("Cleveland")
    // Hello Cleveland!

or methods to classes and objects:

    class Name(first:String, last:String) {
      def speak() = println("Watashi ha " + first + " " + last + " desu.")
    }
    
    object Name {
      def apply(first:String, last:String) = new Name(first,last)
    }
    
    val me = Name("mike", "fogus")
    me speak
    // Watashi ha mike fogus desu.

Scala function/method return type is that of the last expression evaluated.  Functions that are meant to have no meaningful return value are typed as `Unit` returns.  Typically `Unit` functions would be used for those that serve only to cause side-effects.  Further, there is sugar for defining `Unit` methods that, to me, is confusing and a source for potentially frustrating bugs:

    class Name(first:String, last:String) {
      def speak() { 
        println("I am " + first + " " + last + ".")
      }
    }
    
    new Name("mike","fogus") speak
    // I am mike fogus.

The confusion derives from the fact that the only difference between this sugared version and the regular version is that there is no `=`!  This is kinda crazy because of the following:

    def wtf(msg:String) {
      "WTF " + msg
    }
    
    wtf ("Where did it go?")

And nothing seems to happen.  The reason is that the sugared syntax eats the intermediate expressions and forces a return value of type `Unit`.  It is very easy to mistake

    def wtf(msg:String) {
      "WTF " + msg
    }
    
... for ...

    def wtf(msg:String) = {
      "WTF " + msg
    }

Where the latter definition will return the `String` as expected.  Reapeatedly running into problems like this is a symptom of *statement derrangement*.  However, you can aleviate this problem by making sure that your methods and functions return maeningfully typed values, and that those values are used in a meaningful way.  By following this approach the type-checker will catch these kinds of mistakes. 

There is a lot going on here that I am glossing over.  Have fun learning it on your own.

### val

A name defined using `val` is constant in that it cannot be reassigned:

    val L = new java.util.ArrayList[String]()
    
    L
    //=> []
    
    L = 138
    // error: reassignment to val

However, `val` in no way makes an immutable instance, only the name `answer` is immutable:

    L.add("Goo!")
    
    L
    //=> ["Goo!"]

### var

A name defined with `var` is transient and can change on a whim:

    var answer = 42
    answer = 138

Of course the type is still maintained:

    answer = "Money"
    // error: type mismatch

### import

Scala's `import` works very similar to that of Java's:

    import java.util.ArrayList
    
    val AL = new ArrayList[String]()

At a superficial level Java's wildcard import declaration `*` is replaced by `_` in Scala[^anaphora].  However, at a deeper level Scala's `import` is lexical by nature:

    def createJucMap() = {        
      import java.util.HashMap    
      new HashMap[String,String]()
    }
    
    val JM = createJucMap()       
    
    val JM2 = new HashMap[String,String]()
    // error: not found: type HashMap

This is pretty handy.  There is [more to `import` than this as outlined at jcranky.com](http://jcranky.com/2009/08/18/import-statements-in-scala/).

[^anaphora]: You will see `_` pop up in Scala in many different places and taking on different meanings depending on context.  This is a huge cause for confusion.

### package

On the surface, Scala's `package` is of the same nature as Java's:

    package foo.bar
    
    class Baz(x:String)

You know, I find packages kinda boring, so I will just [let David MacIver take over from here](http://www.drmaciver.com/2009/07/how-packages-work-in-scala/).  Scala 2.8 also introduced [package objects](http://www.scala-lang.org/docu/files/packageobjects/packageobjects.html) that are pretty nice.

Methods
-------

There is nothing particularly compelling about Scala's notion of the method beyond the fact that they are pervasive.

### Infix notation for method calls

However, any method that takes either none or a single argument can be used without the dot operator:

    class Bug {                                              
      def buzz(t:Int) = { 
        print("buzz " * t)
        this
      }
    
      def die() = println("Blargh!")
    }
    
    val fly = new Bug
    
    fly buzz 5
    // buzz buzz buzz buzz buzz
    //=> Bug@87342819749
    
    fly die
    // Blargh!
    
    fly buzz 5 die
    // buzz buzz buzz buzz buzz Blargh!

*point of note: Attempting to chain a bunch of these types of these functions will eventually confuse the compiler requiring high-level techniques to fix.  However, I'm not going to talk about that yet.*

Simple closures
---------------

There are different types of name bindings in Scala, but for the sake of narrowing this section I will only talk about bound and free names within the context of functions.  A binding occurs when its value is given as a parameter to the function or explicitly assigned in the function body.  All of these are examples of bindings:

    def chump(x:Int) = {
      val y = 2         
      
      x * y
    }
    
    chump(10)
    //=> 20

Both the names `x` and `y` refer to bindings that were either explicit or occurred as parameters to the function `chump`.  However, a free binding is one that is defined through neither of these means:

    def chimp(x:Int) = {
      x * ub
    }
    
    // error: not found: value ub

The name `ub` is not known at the time that `chimp` is defined, so Scala has no idea what to do with it.  However, what happens if it *is* defined?

    val ub = 2
    
    def chimp(x:Int) = {
      x * ub
    }
    
    chimp(100)
    //=> 200

The name `ub` has now been "captured" by `chimp` and can be referred to within its body.  However, this example is far from compelling because I've only shown that globals can be used in a function body.  A more interesting example would be to try and "capture" a binding that is known to have a limited lifetime (or extent).

    def timesN(n:Int) = (x:Int) => n * x
    
    val times2 = timesN(2)
    
    times2(1000)
    //=> 2000

So a closure therefore is the capturing of free bindings (in this case `n`) within the body of a function allowing its use beyond the confines defined by the normal lifetime (in this case, the body of `timesN`).

*important terms: [binding](http://en.wikipedia.org/wiki/Name_binding), [free bindings](http://en.wikipedia.org/wiki/Free_variables), [extent](http://en.wikipedia.org/wiki/Variable_(programming))*


Applicative programming
-----------------------

At the most basic level of understanding regarding functional programming one must minimally grasp the idea of *applicative programming*.

Consider a list of Integers:

    val L = List(1,2,3,4,5)

*important terms: [higher-order function](http://en.wikipedia.org/wiki/Higher-order_function), [applicative programming](http://en.wikipedia.org/wiki/Applicative_programming_language)*

### `map`

The simplest applicative higher-order method is `map`, used as such:

    L.map(e => 2 * e)
    //=> List(2, 4, 6, 8, 10)

That is, `map` takes a function and applies it to every value in a collection, returning a new collection of the resulting values.

### `filter`

The `filter` method takes a function and returns a collection of only the containing values for which said function returns `true`:

    // take only the even numbers
    
    L.filter(e => e % 2 == 0)
    //=> List(2, 4)

### `reduceLeft`

The `reduceLeft` method takes a function of two arguments and returns the cumulative result of repeatedly applying said function to each element and each intermediate result:

    L.reduceLeft(_ * _)
    //=> 120

The `(_ * _)` construct is idiomatic if not obtuse in Scala and is [explained nicely elsewhere](http://www.codecommit.com/blog/scala/quick-explanation-of-scalas-syntax).


Comprehensions
--------------

Comprehensions in Scala are analogous to set notation in mathematics.

*important terms: [list comprehension](http://en.wikipedia.org/wiki/List_comprehension), [set notation](http://en.wikipedia.org/wiki/Set-builder_notation), [guard clause](http://en.wikipedia.org/wiki/Guard_(computing))*

### for-expressions

At its simplest manifestation, a `for` comprehension operates as follows:

    for (e <- L) yield 2 * e
    //=> List(2, 4, 6, 8, 10)

This results in the same as the `map` example above.

However, `for` also has a guard clause available that can be used to constrain the values for which the comprehension occurs:

    for (e <- L; if e % 2 == 0) yield e 
    //=> List(2, 4)

This results in the same as the `filter` example above.

Finally, `for` can also operate with multiple values:

    for (e  <- L; 
         e2 <- for (a <- L) yield e * a) yield 
      Pair(e, e2)
    
    //=> List((1,1), (1,2),  (1,3),  (1,4),  (1,5), 
              (2,2), (2,4),  (2,6),  (2,8),  (2,10), 
              (3,3), (3,6),  (3,9),  (3,12), (3,15), 
              (4,4), (4,8),  (4,12), (4,16), (4,20), 
              (5,5), (5,10), (5,15), (5,20), (5,25))

The `for` comprehension's syntax is far richer than I am willing to enumerate here, but [others have done a great job instead](http://creativekarma.com/ee.php/weblog/comments/the_scala_for_comprehension_from_a_java_perspective/).  While the `for` can be used in an imperative fashion, I would highly recommend avoiding doing that.  Instead, `for` should **always** be used as a value-producing expression.  Finally, there are ways to [extend your own types to operate within the `for` comprehension](#defining-mapflatmapwithfilter-for-new-kinds-of-for-expressions), but that's a task for later.

Useful Resources
----------------

- [First Steps to Scala](http://www.artima.com/scalazine/articles/steps.html) by Bill Venners


Level A2: Intermediate application programmer
=============================================

Pattern matching
----------------

[Pattern matching is a conditional expression](http://blog.fogus.me/2011/01/14/pattern-matching-vs-destructuring-electric-boogaloo/) able to reach into classes and structures and make decisions and return values based on their contents.  Let's take a look at a simple example:

    def secondThird(l:List[Int]) = l match {
      case f::s::t::rest => List(s,t)
      case _ => List()
    }
    
    secondThird( List(1,2,3,4,5) )
    //=> List(2, 3)
    
    secondThird( List(1,2) ) 
    //=> List()

What `secondThird` says is that *if an `Integer List` has a 2nd and 3rd element then return an `Integer List` of those two elements, otherwise return an empty `Integer List`*.  Pattern matching allows one to use a compact and expressive form for stating your conditional.  How would `secondThird` look if it was "expanded" into the precise conditional denotation?

    def secondThirdExplicit(l:List[Int]) = {
      if (l.length >= 3)
        List(l(1), l(2))
      else
        List()
    }
    
    secondThirdExplicit( List(1,2,3) )
    //=> List(2, 3)
    
    secondThirdExplicit( List(1,2) )  
    //=> List()

For a simple pattern match as needed by `secondThird` there is little compelling reason to use it over the explicit conditional expression.  However, you will find that the complexity of nesting the condition checks will quickly explode.

*TODO: more complex example, match as expression, match case class*

Trait composition
-----------------

Scala Traits are quite powerful.  This is the understatement of the year, or maybe it's reserved by design -- you'll be the judge.  

I gave a talk about this technique at [CUFP 2010](http://cufp.org)(see below).  Unfortunately the 30-minute window did not allow me to get into details (nor was my talk structured in such a way), but maybe it will give a feel for how one might go about using trait composition.

[Click here to watch the video](http://blog.fogus.me/2011/01/10/dancing-monkey-gibbers-on-about-scala-and-clojure)

Please excuse the gibbering.

Recursion
---------

### Mundane recursion

A *mundane* recursive call is one that does not occur in the tail position of a function/method:

    def pack[A](L:List[A]): List[List[A]] = {
      if (L.isEmpty)
        List(L)
      else {
        val (packed, next) = L span { _ == L.head }

        if (next == Nil) 
          List(packed)
        else 
          packed :: pack(next)
      }
    }
    
    /* => List(List('a, 'a, 'a, 'a), 
               List('b), 
               List('c, 'c), 
               List('a, 'a), 
               List('d), 
               List('e, 'e, 'e, 'e)) */

The implementation of `pack`[^pack] has its recursive call in the body of the cons operator `::` and as a result is limited to processing `List`s bounded by the JVM's stack limit.  This type of function is known as an *accumulating recursive function*.  The accumulation in this case is the resulting packed `List`.  These types of accumulating recursive functions can be transformed into tail-recursive versions using a *helper function*.

[^pack]: The mundane implementation of `pack` was taken from <http://aperiodic.net/phil/scala/s-99/>.

### Tail recursion

Here is the transformed tail-recursive version of `pack`:

    import scala.annotation.tailrec
    
    def pack[A](list:List[A]): List[List[A]] = {
      // Local helper function
      
      @tailrec def P[A](L:List[A], Acc:List[List[A]]):List[List[A]] = {
        if (L.isEmpty) {
          List(L)
        }
        else {
          val (packed, next) = L span { _ == L.head }
          
          if (next == Nil) 
            Acc ++ List(packed)
          else 
            P(next, Acc ++ List(packed))
        }
      }
    
      P(list, List())
    }

Simple no?  See *[Common Lisp: A Gentle Introduction to Symbolic Computation](http://www.cs.cmu.edu/~dst/LispBook/)* for more information about this transformation technique.

It's a good idea to use the `@tailrec` annotation when creating tail recursive functions as its purpose is to allow you to gain some insight should a function that you expect to be optimized is not.  [Rich Dougherty does a good job explaining @tailrec](http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html), so I'll avoid repeating all of his points here.  However, let's see what happens when you mark a function with `@tailrec` that is not tail recursive:

    @tailrec def factorial(n: Int): Int = {
      if (n <= 1) 1
      else n * factorial(n - 1)
    }
    
    // error: could not optimize @tailrec annotated method: 
    // it contains a recursive call not in tail position

I tend to like this type of explicit tagging of code optimizations, but maybe that's just me.

#### Question

In the tail-recursive version of `pack`, why did I use `Acc ++ List(packed)` to build the accumulator rather than `Acc ::: List(packed)`?

### Tail calls

TBD

XML literals
------------

Scala's XML literals are a bitter sweet feature for me, but thankfully half of the reasons are [purely personal](http://www.scala-lang.org/node/292).  As a quick pass, observe the following:

    def wrap(id: String, name:String, age:Int) = {
      <person id={id}>
        <name>{name}</name>
        <age>{age}</age>
      </person>
    }
    
    wrap("fog1", "Fogus", 138)                        
    //=> <person id="fog1">
           <name>Fogus</name>
           <age>138</age>
         </person>

Pretty cool right?  You'd think so.  However, Scala's XML support is almost universally regarded as the red-headed step-child of the language and therefore its foibles, and dare I say completely effed parts, are scarely attended to.  However, there is light at the end of the `<tunnel/>` as [Daniel Spiewak has set out to correct Scala's XML support](https://github.com/djspiewak/anti-xml), and in all likelihood will rock its world.  Speaking of Mr. Spiewak, his [post on the XML support is brilliant](http://www.codecommit.com/blog/scala/working-with-scalas-xml-support) and is much better than I could ever pull off.  Go there instead.

Useful Resources
----------------

- [Pattern Matching vs. Destructuring… Electric Boogaloo](http://blog.fogus.me/2011/01/14/pattern-matching-vs-destructuring-electric-boogaloo/)
- [Why Object-Oriented Languages Need Tail Calls](http://projectfortress.sun.com/Projects/Community/blog/ObjectOrientedTailRecursion) by Guy L. Steele Jr.
- [Effective Java](http://www.amazon.com/o/asin/0321356683?tag=fogus-20) by Joshua Bloch
- [Common Lisp: A Gentle Introduction to Symbolic Computation](http://www.cs.cmu.edu/~dst/LispBook/)
- [Programming Scala](http://programming-scala.labs.oreilly.com/) by Wampler and Payne


Level A3: Expert application programmer 
=======================================

Folds
-----

### `foldLeft`

### `foldRight`


Streams and other lazy data structures
--------------------------------------

TBD

Actors
------

TBD

Combinator parsers
------------------

TBD

Useful Resources
----------------

- The [Akka library](http://akka.io/)
- [Scala Parser Combinators](http://jim-mcbeath.blogspot.com/2008/09/scala-parser-combinators.html) by Jim McBeath (also has an extensive list to other posts on the same topic)


Level L1: Junior library designer
=================================

Scala is a language with a central philosophy that "libraries are king".  That is, where many languages provide language features at the level of the language itself, Scala instead provides a base (both semantically and syntactically) that facilitates features as libraries.  Consider the following list[^oder-feats] of features in Scala -- how are these same features exposed by Java, C#, C++, or any number of popular languages?

- No enums, there's just an Enumeration object in the standard library
- No primitive operators, they are methods
- No separate rules for annotations, they are constructors
- No syntax for asserts, they are methods
- No complicated rules for statics, they are objects
- No autoboxing, implicits do that job for you
- No numeric conversion rules, implicits to that also
- No collection literals as in Groovy, Clojure and future Java, it's just objects and method calls

[^oder-feats]: This list is taken from [Dr. Odersky's comment at theserverside.com](http://www.theserverside.com/news/thread.tss?thread_id=62010#344541)

This is quite a list!  Indeed, the list in emblematic of the dirving force behind Scala library construction.  As a result, creating libraries in Scala is a task not to be taken lightly.  It's important that any library that you might devise should be created thoughtfully to promote maximum reuse potential.

Type parameters
---------------

TBD

TODO: Decipher <http://www.scala-lang.org/node/129>

Traits
------

TBD

Lazy vals
---------

TBD

Control abstraction via currying
--------------------------------

TBD 

By-name parameters
------------------

TBD

Useful Resources
----------------

- [Types and Programming Languages](http://www.amazon.com/o/asin/0262162091?tag=fogus-20) by Benjamin Pierce


Level L2: Senior library designer
=================================

Variance annotations
--------------------

TBD

Existential types 
-----------------

*e.g., to interface with Java wildcards*

TBD

Self type annotations and the cake pattern for dependency injection
-------------------------------------------------------------------

TBD

Structural types
----------------

TBD

Defining map/flatmap/withFilter for new kinds of for-expressions
----------------------------------------------------------------

TBD 

Extractors
----------

TBD

TODO: Worth drawing a comp to destructuring?

Useful Resources
----------------

- [Basic Category Theory for Computer Scientists](http://www.amazon.com/o/asin/0262660717?tag=fogus-20) by Benjamin Pierce


Level L3: Expert library designer
=================================

Early initializers
------------------

TBD

Abstract types
--------------

TBD

Implicit definitions
--------------------

TBD

Higher-kinded types
-------------------

TBD

Useful Resources
----------------

- [Advanced Topics in Types and Programming Languages](http://www.amazon.com/o/asin/0262162288?tag=fogus-20) by Benjamin Pierce
- The [Haskell programming language](http://www.haskell.org/)


Level Z
=======

Awareness of the limitations of early initializers
--------------------------------------------------

TBD

Purely functional data structures
---------------------------------

TBD

Type-level tranformations
-------------------------

TBD

Spec-based testing
------------------

TBD

Practical algebraic structures of category theory
-------------------------------------------------

What the heck is category theory?  It's best that you [get a good book](http://www.amazon.com/o/asin/0262660717?tag=fogus-20) on the topic and dive in yourself.  You can also read some interesting papers, but it will help tremendously to know the following minimally.

### Components of category theory

- objects 
    + not what you think
    + more like sets
- arrows 
    + sometimes called morphisms
- composition operator
- identity operator 
    + one per operator

Other useful terms are *domain* and *codomain* of arrows -- what we would commonly know as domain (inputs kinda) or range (returns kinda).

### Morphisms

Operations (functions) on objects (sets).  Think of the following picture:

    A  -------------> B
             M

`A` is an object can be transformed into `B` via a structure-preserving morphism `M`.

**TODO**: go a little deeper into "structure-preserving"

### Composition of morphisms

The composition of a morphism only makes sense when the codomain of the first is in the domain of the second.

**TODO**: more

### Partial morphism

A morphism `M'` whose domain is a partial set of the domain of `M`.

**TODO**: more

### Homomorphism (functor)

Builds new categories from existing categories.  This is the crux of the notion of *higher-order kinds*, of which [Daniel Spiewak gives a nice introduction to](http://assets.en.oreilly.com/1/event/45/High%20Wizardry%20in%20the%20Land%20of%20Scala%20Presentation.pdf) (PDF).

**TODO**: more on type constructors and HOKs

### Monoid

A category with one object.

**TODO**: more

### Isomorphism

A morphism that has an inverse such that its inverse composed is its identity.  (huh?)

**TODO**: clarify + more

### Isomorphic

Two objects are isomorphic if an isomorphism exists between them:

    A -----> B
         M
    A <----- B

`A` and `B` are isomorphic via `M`.

**TODO**: more


### Injective functions

Every element of a function's domain maps to only one element of its codomain, but not necessarily all.

**TODO**: more + pics

### Surjective functions

Every element of a function's codomain has at least one mapping (but maybe more) from elements in its domain.

**TODO**: more + pics

### Bijective functions

A function is both injective and surjective.

**TODO**: more + pics

### Products

TBD

### Implementing practical structures

TBD

### Resources

- [Categories, Types, and Structures](http://www.amazon.com/o/asin/0262011255?tag=fogus-20) by Longo and Asperti
- [Arrows, Structures, and Functors](http://www.amazon.com/o/asin/0120590603?tag=fogus-20) by Arbib and Manes

These are both rare, but worth the effort in locating.  I can't say that a fully understand them, but I'm working on it.

An understanding of type theory
-------------------------------

There is no possible way that I could express even the gist of type theory here.  Like anything worthwhile, learning type theory is difficult.  There are many reasons for this, but chief among them is that type theory looks a lot like set theory but is more abstract.  To try and map the tenats of set theory on to type theory will prove to be less than satisfactory.

TBD

Abstraction in the face of limited laziness
-------------------------------------------

TBD

Useful Resources
----------------

- [Purely Functional Datastructures](http://www.amazon.com/o/asin/0521663504?tag=fogus-20) by Chris Okasaki
- The [scalaz library](https://github.com/scalaz/scalaz)
- *Using Category Theory to Design Implicit Conversions and Generic Operators* by John Reynolds


Level _: Missing Concepts
=========================

Type Inference 
--------------

TBD

Type Classes
------------

**TODO**: Dig out note one the Ghosh post(s)


DSLs 
----

TBD


Footnotes
=========

