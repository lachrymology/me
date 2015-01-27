- Web Programming
- Introduction to Continuations: Web Programming
- More Web Transformations
- Highlevel Overview on Continuations
- Implementing an Automatic Continuation Converter
- Continuations as a Language Feature
- Continuations in Racket
- Playing with Continuations
- Continuation Conclusions

========================================================================
>>> Web Programming

[[[ PLAI Chapter 15 ]]]

Consider web programming as a problem that arises frequently.  The HTTP
protocol is *stateless*: each HTTP query can be thought of as running a
program (or a function), getting a result, then killing it.  This makes
interactive applications hard to write.

For example, consider this behavior:

* You go on a flight reservation website, and look at flights to
  Paris or London for a vacation.

* You get a list of options, and choose one for Paris and one for
  London, control-click the first and then the second to open them in
  new tabs.

* You look at the descriptions and decide that you like the first one
  best, so you click the button to buy the ticket.

* A month later you go on your plane, and when you land you realize that
  you're in the wrong country -- the ticket you payed for was the second
  one after all...

Obviously there is some fundamental problem here -- especially given
that this problem plagued many websites early on (and these days these
kind of problems are still there, except that people are much more aware
of it, and are much more prepared to deal with it).  In an attempt to
clarify what it is exactly that went wrong, we might require that each
interaction will result in something that is deterministically based on
what the browser window shows when the interaction is made -- but even
that is not always true.  Consider the same scenario except with a
bookstore and an "add to my cart" button.  In this case you *want* to be
able to add one item to the cart in the first window, then switch to the
second window and click "add" there too: in this case you want to end up
with a cart that has both items.

The basic problem here is HTTP's statelessness, something that both web
servers and web browsers use extensively.  Browsers give you navigation
buttons and sometimes will not even communicate with the web server when
you use them (instead, they'll show you cached pages), they give you the
ability to open multiple windows or tabs from the current one, and they
allow you to "clone" the current tab.  If you view each set of HTTP
queries as a session -- this means that web browsers allow you to go
back and forth in time, explore multiple futures in parallel, and clone
your current world.

These are features that the HTTP protocol intentionally allows by being
stateless, and that people have learned to use effectively.  A stateful
protocol (like ssh, or ftp) will run in a single process (or a program,
or a function) that is interacting with you directly, and this process
dies only when you disconnect.  A big advantage of stateful protocols is
their ability to be very interactive and rely on state (eg, an editor
updates a character on the screen, relying on the rest of it showing the
same text); but stateless protocols can scale up better, and deal with a
more hectic kind of interaction (eg, open a page on an online store,
keep it open and buy the item a month later; or any of the above "time
manipulation" devices).

[Side-note: Some people think that Ajax is the answer to all of these
problems.  In reality, Ajax is layered on top of (asynchronous) web
queries, so in fact it is the exact same situation.  You do have an
option of creating an application that works completely on the client
side, but that wouldn't be as attractive -- and even if you do so,
you're still working inside a browser that can play the same time
tricks.]

========================================================================
>> Basic web programming

[[[ PLAI Chapter 16 ]]]

Obviously, writing programs to run on a web server is a profitable
activity, and therefore highly desirable.  But when we do so, we need to
somehow cope with the web's statelessness.  To see the implications from
a PL point of view we'll use a small "toy" example that demonstrates the
basic issues -- an "addition" service:

- Server sends a page asking for a number,
- User types a number and hits enter,
- Server sends a second page asking for another number,
- User types a second number and hits enter,
- Server sends a page showing the sum of the two numbers.

[Such a small applications are not realistic, of course: you can
obviously ask for both numbers on the same page.  They are very
effective, though, in minimizing the general problem of web interaction
to a more comprehensible small core problem.]

Starting from just that, consider how you'd *want* to write the code for
such a service.  (If you have experience writing web apps, then try to
forget it for now.)

  (web-display
    (+ (web-read "First number")
       (web-read "Second number")))

But this is never going to work.  The interaction is limited to
presenting the user with some data and that's all -- you cannot do any
kind of interactive querying.  We therefore must turn this server
function into three separate functions: one that shows the prompt for
the first number, one that gets the value entered and shows the second
prompt, and a third that shows the results page.  Assuming a generic
"query argument" that represents the browser request, and a return value
that represents a page for the browser to render, we have:

  (define (f1 query)
    ... show the first question ...)

  (define (f2 query)
    ... extract the number from the query ...
    ... show the second question ...)

  (define (f3 query)
    ... extract the number from the query ...
    ... show the sum ...)

Note that `f2' receives the first number directly, but `f3' doesn't.
Yet, it is obviously needed to show the sum.  A typical hack to get
around this is to use a "hidden field" in the HTML form that `f2'
generates, where that field holds the second result.  To make things
more concrete, we'll use some imaginary web API functions:

  (define (f1 query)
    (web-read "First number" 'n1 "f2"))

  (define (f2 query)
    (let ([n1 (get-field query 'n1)])
      (with-hidden-field 'n1 n1
        (web-read "Second number" 'n2 "f3"))))

  (define (f3 query)
    (web-display
      "Your two numbers sum up to: "
      (+ (get-field query 'n1)
         (get-field query 'n2))))

Which would (supposedly) result in something like the following html
forms when the user enters 1 and 2:

   <form action="http://.../f2">
     <input type="text" name="n1" />
   </form>

   <form action="http://.../f3">
     <input type="hidden" name="n1" value="1" />
     <input type="text" name="n2" />
   </form>

   <form action="http://.../f1">
     <input type="text" name="result" value="3" readonly />
   </form>

This is often a bad solution: it gets very difficult to manage with real
services where the "state" of the server consists of much more than just
a single number -- and it might even include values that are not
expressible as part of the form (for example an open database connection
or a running process).  Worse, the state is all saved in the client
browser -- if it dies, then the interaction is gone.  (Imagine doing
your taxes, and praying that the browser won't crash a third time.)

Another common approach is to store the state information on the server,
and use a small handle (eg, in a cookie) to identify the state, then
each function can use the cookie to retrieve the current state of the
service -- but this is exactly how we get to the above bugs.  It will
fail with any of the mentioned time-manipulation features.

========================================================================
>>> Introduction to Continuations: Web Programming

To try and get a better solution, we'll re-start with the original
expression:

  (web-display (+ (web-read "First number")
                  (web-read "Second number")))

and assuming that `web-read' works, we need to begin with executing the
first read:

                  (web-read "First number")

Then we need to take that result and plug it into an expression that
will read the second number and sum the results -- that's the same as
the first expression, except that instead of the first `web-read' we use
a "hole":

  (web-display (+ <*>
                  (web-read "Second number")))

where `<*>' marks the point where we need to plug the result of the
first question into.  A better way to explain this hole is to make it a
function argument:

  (lambda (<*>)
    (web-display (+ <*>
                    (web-read "Second number"))))

Actually, we can split the second and third steps in the same way.
First see how the first step is combined with the second "consumer"
function:

  ((lambda (<*>)
     (web-display (+ <*> (web-read "Second number"))))
   (web-read "First number"))

And now we can continue doing this and split the body of the consumer:

     (web-display (+ <*> (web-read "Second number")))

into a "reader" and the rest of the computation (using a new hole):

                         (web-read "Second number")   ; reader part

     (web-display (+ <*> <*2>))                       ; rest of comp

Doing all of this gives us:

  ((lambda (<*1>)
     ((lambda (<*2>)
        (web-display (+ <*1> <*2>)))
      (web-read "Second number")))
   (web-read "First number"))

And now we can proceed to the main trick.  Conceptually, we'd like to
think about `web-read' as something that is implemented in a simple way:

  (define (web-read prompt)
    (printf "~a: " prompt)
    (read-number))

To accommodate the above "hole functions", we change it by adding an
argument for the consumer function that we need to pass the result to,
and call it `web-read/k':

  (define (web-read/k prompt k)
    (display prompt)
    (k (read-number)))

[In this version of `web-read' the `k' argument is the *continuation* of
the computation.  (`k' is a common name for a continuation argument.)]

This is not too different from the previous version -- the only
difference is that we make the function take a consumer function as an
input, and hand it what we read instead of just returning it.  Using it
makes things a little easier, since we pass the hole function which gets
called automatically, instead of combining things ourselves:

  (web-read/k "First number"
    (lambda (<*1>)
      (web-read/k "Second number"
        (lambda (<*2>)
          (web-display (+ <*1> <*2>))))))

You might notice that this looks too complicated; we could get exactly
the same result with:

  (web-display (+ (web-read/k "First number" (lambda (<*>) <*>))
                  (web-read/k "Second number" (lambda (<*>) <*>))))

but then there's not much point to having `web-read/k' at all...  So why
have it?  Because using it, we can make each application of `web-read/k'
be one of these server functions, where the computation dies after the
interaction.  The only thing that `web-read/k' needs to worry about is
storing the receiver function in a hash table somehow so it can be
retrieved when the user is done with the interaction.  (This means that
the "submit" button will somehow encode a reference to the hash table
that can make the next service call retrieve the stored function.)  The
`web-read/k' format is fitting for such an interaction, because at each
step just the reading is happening -- everything else is put inside the
consumer function.

========================================================================
>> Simulating web reading

We can actually try all of this in plain Racket by simulating web
interactions.  This is useful to look at the core problem while avoiding
the whole web mess that is uninteresting for this discussion.  The main
feature that we need to emulate is statelessness -- and we can achieve
that using `error' to guarantee that each interaction is properly
killed.  We will do this in `web-display' which simulates sending the
results to the client and therefore terminating the program on the
server.  More importantly, we need to do it in `web-read/k' -- in this
case, the termination happens after we save the requested receiver in a
`what-next' box.  Instead of storing just the receiver there, we will
store a function that does the prompting and the reading and then invoke
the receiver.  `what-next' is therefore bound to a box that holds a
no-argument function that does the work of resuming the computation, and
when there is nothing next, it is bound to a `nothing-to-do' function
that throws an appropriate error.  `resume' simply invokes the current
`what-next'.

  (define (nothing-to-do)
    (error "There is no computation in progress now."))

  (define what-next (box nothing-to-do))

  (define (web-display n)
    (set-box! what-next nothing-to-do)
    (error 'web-output "~s" n))

  (define (web-read/k prompt k)
    ;; note that a more accurate web-like behavior would print the
    ;; prompt before aborting, but this doesn't change anything
    ;; important, so we go with a more convenient time to show it
    (set-box! what-next
              (lambda ()
                (printf "~a: " prompt)
                (k (read))))
    (error 'web-read/k "enter (resume) to continue"))

  (define (resume)
    ;; to avoid mistakes, we clear out `what-next' before invoking it
    (let ([next (unbox what-next)])
      (set-box! what-next nothing-to-do)
      (next)))

We can now try out our code for the addition server, but switch to plain
argument names instead of those `<*>'s:

  (web-read/k "First number"
    (lambda (n1)
      (web-read/k "Second number"
        (lambda (n2)
          (web-display (+ n1 n2))))))

and see how everything works.  You can also try now the bogus expression
that we mentioned:

  (web-display (+ (web-read/k "First number" (lambda (n) n))
                  (web-read/k "Second number" (lambda (n) n))))

and see how it breaks.

========================================================================

If we now sit back for a minute and checkout what we actually did at a
higher level, it should ring a bell.  We've taken a simple compound
expression and "linearized" it as a sequence of an input operation and a
continuation receiver for its result.  This is essentially the same
thing that we did to deal with IO in the lazy language -- and the
similarity is not a coincidence.  The problem that we faced there was
very different (representing IO as values that describe it), but it
originates from a similar situation -- some computation goes on (in
whatever way the lazy language decides to evaluate it), and when we have
a need to read something we must return a description of this read that
contains "the rest of the computation" to the eager part of the
interpreter that executes the IO.  Once that part has the user input, it
sends it to this computation remainder, which can return another read
request, and so on.

Based on this intuition, we can guess that this can work for any piece
of code, and that we can even come up with a nicer "flat" syntax for it.
For example, here is a simple macro that flattens a sequence of reads
and a final display:

  (define-syntax web-code
    (syntax-rules (read display)
      [(_ (read n prompt) more ...)
       (web-read/k prompt
         (lambda (n)
           (web-code more ...)))]
      [(_ (display last))
       (web-display last)]))

and using it:

  (web-code (read x "First number")
            (read y "Second number")
            (display (+ x y)))

However, we'll avoid such cuteness to make the transformation more
explicit for the sake of the discussion.  Eventually, we'll see how
things can become even better than that in Racket: we can get to write
plain-looking Racket expressions and avoid even the need for an
imperative form for the code.  In fact, it's easy to write this addition
server using Racket's web server framework, and the core of the code
looks very simple:

  (define (start initial-request)
    (page "The sum is: "
          (+ (web-read "First number")
             (web-read "Second number"))))

There is not much more than that -- it has two utilities, `page' creates
a well-formed web page, and `web-read' performs the reading.  The main
piece of magic there is in `send/suspend' which makes the web server
capture the computation's continuation and store it in a hash table, to
be retrieved when the user visits the given URL.  Here's the full code:

  ----------------------------------------------------------------------
  #lang web-server/insta
  (define (page . body)
    (response/xexpr
     `(html (body ,@(map (lambda (x) (if (number? x) (format "~a" x) x))
                         body)))))
  (define (web-read prompt)
    ((compose string->number (curry extract-binding/single 'n)
              request-bindings send/suspend)
     (lambda (k)
       (page `(form ([action ,k])
                ,prompt ": " (input ([type "text"] [name "n"])))))))
  (define (start initial-request)
    (page "The sum is: "
          (+ (web-read "First number")
             (web-read "Second number"))))
  ----------------------------------------------------------------------

========================================================================
>>> More Web Transformations

[[[ PLAI Chapter 17 ]]]

>> Transforming a recursive function

We did the above transformation on a simple expression -- and as you'd
guess, it's possible to make it work for recursive functions too,
although it gets a little tricky.  As done above, we start with some
plain looking code:

  (define (sum prompts)
    (if (null? prompts)
      0
      (+ (web-read (first prompts))
         (sum (rest prompts)))))

Further, we want this function to be usable as a library function -- it
should be useful for web applications that need this functionality.  One
result of this is that it doesn't `web-display' its result.

We begin by converting the `web-read' to its continuation version:

  (define (sum prompts)
    (if (null? prompts)
      0
      (web-read/k (first prompts)
        (lambda (n)
          (+ n
             (sum (rest prompts)))))))

But using `web-read/k' immediately terminates the running computation,
so this won't work.  Another way to see the problem is that the
continuation input to `web-read/k' should have the rest of the
computation, yet in this version it only has the addition -- if `sum' is
used as in some larger context as a library function, then that whole
context will be lost.  The way to solve this is to make `sum' itself
take a continuation, which we'll get in a similar way -- by rewriting it
as a `sum/k' function:

  (define (sum/k prompts k)
    (if (null? prompts)
      0
      (web-read/k (first prompts)
        (lambda (n)
          (+ n
             (sum (rest prompts)))))))

We also need to deal with the recursive `sum' call and change it to a
`sum/k'.  Clearly, the continuation is the same continuation that the
original sum was called with, so we need to pass it on in the recursive
call too:

  (define (sum/k prompts k)
    (if (null? prompts)
      0
      (web-read/k (first prompts)
        ;; get the value provided by the user, and add it to the value
        ;; that the recursive call generates
        (lambda (n)
          (+ n
             (sum/k (rest prompts)
                    k))))))

But there is another problem now: the addition is done outside of the
continuation, therefore it will be lost as soon as there's a second
`web-read/k' call.  In other words, computation bits that are outside of
any continuations are going to disappear, and therefore they must be
encoded as an explicit part of the continuation.  The solution is
therefore to move the addition *into* the continuation:

  (define (sum/k prompts k)
    (if (null? prompts)
      0
      (web-read/k (first prompts)
        (lambda (n)
          (sum/k (rest prompts)
                 (lambda (sum-of-rest)
                   (k (+ n sum-of-rest))))))))

Note that with this code every new continuation is bigger -- it contains
the previous continuation (note that "contains" here is done by making
it part of the closure), and it also contains one new addition.

But if the continuation is only getting bigger, then how do we ever get
a result out of this?  Put differently, when we reach the end of the
prompt list, what do we do?  -- Clearly, we just return 0, but that
silently drops the continuation that we worked so hard to accumulate.
This means that just returning 0 is wrong -- instead, we should send the
0 to the pending continuation:

  (define (sum/k prompts k)
    (if (null? prompts)
      (k 0)
      (web-read/k (first prompts)
        (lambda (n)
          (sum/k (rest prompts)
                 (lambda (sum-of-rest)
                   (k (+ n sum-of-rest))))))))

This makes sense now: this `sum/k' is a utility to be used in a web
server application, and such applications need to be transformed in a
similar way to what we're doing.  Therefore, our own `sum/k' is a
function that expects to be invoked from such transformed code -- so it
needs to have an argument for the waiting receiver, and it needs to pass
that receiver around (accumulating more functionality into it) until
it's done.

To try it, we need a top-level consumer value to pass on to the `sum/k'
call.  The obvious choice in this case is to use `web-display':

  (sum/k '("First" "Second" "Third")
         (lambda (sum) (web-display sum)))

or, more simply:

  (sum/k '("First" "Second" "Third")
         web-display)

========================================================================
>> Using `sum/k'

To get some more experience with this transformation, we'll try to
convert some code that uses the above `sum/k'.  For example, lets add a
multiplier argument that will get multiplied by the sum of the given
numbers.  Begin with the simple code.  This is an actual application, so
we're writing just an expression to do the computation and show the
result, not a function.

  (web-display (* (web-read "Multiplier")
                  (sum '("First" "Second" "Third"))))

We now need to turn the two function calls into their `*/k' form.  Since
we covered `sum/k' just now, begin with that.  The first step is to
inspect its continuation: this is the same code after we replace the
`sum' call with a hole:

  (web-display (* (web-read "Multiplier")
               <*>))

Now take this expression, make it into a function by abstracting over
the hole and call it `n', and pass that to `sum/k':

  (sum/k '("First" "Second" "Third")
         (lambda (n)
           (web-display (* (web-read "Multiplier")
                           n))))

(Note that this is getting rather mechanical now.)  Now for the
`web-read' part, we need to identify its continuation -- that's the
expression that surrounds it inside the first continuation function, and
we'll use `m' for the new hole:

           (* m
              n)

As above, abstract over `m' to get a continuation, and pass it into
`web-read/k':

  (sum/k '("First" "Second" "Third")
         (lambda (n)
           (web-read/k "Multiplier"
                       (lambda (m)
                         (web-display (* m n))))))

and we're done.  An interesting question here is what would happen if
instead of the above, we start with the `web-read' and *then* get to the
`sum'?  We'd end up with a different version:

  (web-read/k "Multiplier"
              (lambda (m)
                (sum/k '("First" "Second" "Third")
                       (lambda (n)
                         (web-display (* m n))))))

Note how these options differ -- one reads the multiplier first, and the
other reads it last.  (Side-note: if in the last step of turning
`web-read' to `web-read/k' we consider the *whole* expression when we
formulate the continuation, then we get to the same code.  But this
isn't really right, since it converts already-converted code.)

In other words, our conversion results in code that fixes a specific
evaluation order for the original expression.  The way that the inputs
happen in the original expression

  (web-display (* (web-read "Multiplier")
                  (sum '("First" "Second" "Third"))))

is unspecified in the code -- it only happens to be left-to-right
implicitly, because Racket evaluates function arguments in that order.
However, the converted code does *not* depend on how Racket evaluates
function arguments.  (Can you see a similar conclusion here about
strictness?)

Note also another property of the converted code: every intermediate
result has a name now.  This makes sense, since another way to fix the
evaluation order is to do just that.  For example, convert the above to
either

  (let* ([m (web-read "Multiplier")]
         [n (sum '("First" "Second" "Third"))])
    (* m n))

or

  (let* ([n (sum '("First" "Second" "Third"))]
         [m (web-read "Multiplier")])
    (* m n))

This is also a good way to see why this kind of conversion can be a
useful tool in compiling code: the resulting code is in a kind of a
low-level form that makes it easy to translate to assembly form, where
function calls are eliminated, and instead there are only jumps (since
all calls are tail-calls).  In other words, the above can be seen as a
piece of code that is close to:

  n = sum(["First","Second","Third"]);
  m = web_read("Multiplier");
  web_display(m*n);

========================================================================
>> Converting stateful code

Another case to consider is applying this transformation to code that
uses mutation with some state.  For example, here's some simple account
code that keeps track of a `balance' state:

  (define account
    (let ([balance (box 0)])
      (lambda ()
        (set-box! balance
                  (+ (unbox balance)
                     (web-read (format "Balance: ~s; Change"
                                       (unbox balance)))))
        (account))))

(Note that there is no `web-display' here, since it's an infinite loop.)
As usual, the fact that this function is expected to be used by a web
application means that it should receive a continuation:

  (define account/k
    (let ([balance (box 0)])
      (lambda (k)
        (set-box! balance
                  (+ (unbox balance)
                     (web-read (format "Balance: ~s; Change"
                                       (unbox balance)))))
        (account))))

Again, we need to convert the `web-read' into `web-read/k' by
abstracting out its continuation.  We'll take the `set-box!' expression
and create a continuation out of it:

        (set-box! balance
                  (+ (unbox balance)
                     <*>))

and using `change' as the name for the continuation argument, we get:

  (define account/k
    (let ([balance (box 0)])
      (lambda (k)
        (web-read/k (format "Balance: ~s; Change"
                            (unbox balance))
                    (lambda (change)
                      (set-box! balance (+ (unbox balance) change))))
        (account))))

And finally, we translate the loop call to pass along the same
continuation it received (it seems suspicious, but there's nothing else
that could be used there):

  (define account/k
    (let ([balance (box 0)])
      (lambda (k)
        (web-read/k (format "Balance: ~s; Change" (unbox balance))
                    (lambda (change)
                      (set-box! balance (+ (unbox balance) change))))
        (account/k k))))

But if we try to run this -- (account/k web-display) -- we don't get any
result at all: it reads one number and then just stops without the usual
request to continue, and without showing any result.  The lack of
printed result is a hint for the problem -- it must be the void return
value of the `set-box!'.  Again, we need to remember that invoking a
`web-read/k' kills any pending computation and the following (resume)
will restart its continuation -- but the recursive call is not part of
the loop.

The problem is the continuation that we formulated:

        (set-box! balance
                  (+ (unbox balance)
                     change))

which should actually contain the recursive call too:

        (set-box! balance
                  (+ (unbox balance)
                     change))
        (account/k k)

In other words, the recursive call was left outside of the continuation,
and therefore it was lost when the fake server terminated the
computation on a `web-read/k' -- so it must move into the continuation
as well:

  (define account/k
    (let ([balance (box 0)])
      (lambda (k)
        (web-read/k (format "Balance: ~s; Change" (unbox balance))
                    (lambda (change)
                      (set-box! balance (+ (unbox balance) change))
                      (account/k k))))))

and the code now works.  The only suspicious thing that we're still left
with is the loop that passes `k' unchanged -- but this actually is the
right thing to do here.  The original loop had a tail-recursive call
that didn't pass along any new argument values, since the infinite loop
is doing its job via mutations to the box and nothing else was done in
the looping call.  The continuation of the original call is therefore
also the continuation of the second call, etc.  All of these
continuations are closing over a single box and this binding does not
change (it *cannot* change if we don't use a `set!'); instead, the boxed
value is what changes through the loop.

========================================================================
>> Converting higher order functions

Next we try an even more challenging transformation: a higher order
function.  To get a chance to see more interesting examples, we'll have
some more code in this case.

For example, say that we want to compute the sum of squares of a list.
First, the simple code (as above, there's no need to wrap a
`web-display' around the whole thing, just make it return the result):

  (define (sum l) (foldl + 0 l))
  (define (square n) (* n n))
  (define (read-number prompt)
    (web-read (format "~a number" prompt)))
  (web-display (sum (map (lambda (prompt) (square (read-number prompt)))
                         '("First" "Second" "Third"))))

Again, we can begin with `web-read' -- we want to convert it to the
continuation version, which means that we need to convert `read-number'
to get one too.  This transformation is refreshingly trivial:

  (define (read-number/k prompt k)
    (web-read/k (format "~a number" prompt) k))

This is an interesting point -- it's a simple definition that just
passes `k' on, as is.  The reason for this is similar to the simple
continuation passing of the imperative loop: the pre-translation
`read-number' is doing a simple tail call to `web-read', so the
evaluation context of the two is identical.  The only difference is the
prompt argument, and that's the same `format' call.

Of course things would be different if `format' itself required a web
interaction, since then we'd need some `format/k', but without that
things are really simple.  The same goes for the two utility functions
-- `sum' and `square': they're not performing any web interaction so it
seems likely that they'll stay the same.

We now get to the main expression, which should obviously change since
it needs to call `read-number/k', so it needs to send it some
continuation.  By now, it should be clear that passing an identity
function as a continuation is going to break the surrounding context
once the running computation is killed for the web interaction.  We need
to somehow generate a top-level identity continuation and propagate it
inside, and the `sum' call should be in that continuation together with
the `web-display' call.  Actually, if we do the usual thing and write
the expression with a `<*>' hole, we get:

  (web-display (sum (map (lambda (prompt) (square <*>))
                         '("First" "Second" "Third"))))

and continuing with the mechanical transformation that we know, we need
to abstract over this expression+hole into a function, then pass it as
an argument to `read-number/k':

  ;; very broken
  (read-number/k
   (lambda (<*>)
     (web-display (sum (map (lambda (prompt) (square <*>))
                            '("First" "Second" "Third"))))))

But that can't work in this case -- we need to send `read-number/k' a
prompt, but we can't get a specific one since there is a *list* of them.
In fact, this is related to a more serious problem -- pulling out
`read-number/k' like this is obviously broken since it means that it
gets called only once, instead, we need to call it once for each prompt
value.

The solution in this case is to convert `map' too:

  (web-display (sum (map/k (lambda (prompt)
                             (square (read-number prompt)))
                           '("First" "Second" "Third")
                           ...some-continuation...)))

and of course we should move `web-display' and `sum' into that
continuation:

  (map/k (lambda (prompt) (square (read-number prompt)))
         '("First" "Second" "Third")
         (lambda (l) (web-display (sum l))))

We can now use `read-number/k', but the question is what should it get
for it's continuation?

  (map/k (lambda (prompt) (square (read-number/k prompt ???)))
         '("First" "Second" "Third")
         (lambda (l) (web-display (sum l))))

Clearly, `map/k' will need to somehow communicate *some* continuation to
the mapped function, which in turn will send it to `read-number/k'.
This means that the mapped function should get converted too, and gain a
`k' argument.  To do this, we'll first make things convenient and have a
name for it (this is only for convenience, we could just as well convert
the `lambda' directly):

  (define (read-squared prompt)
    (square (read-number/k prompt ???)))
  (map/k read-squared
         '("First" "Second" "Third")
         (lambda (l) (web-display (sum l))))

Then convert it in the now-obvious way:

  (define (read-squared/k prompt k)
    (read-number/k prompt
                   (lambda (n)
                     (k (square n)))))
  (map/k read-squared/k
         '("First" "Second" "Third")
         (lambda (l) (web-display (sum l))))

Everything is in place now -- except for `map/k', of course.  We'll
start with the definition of plain `map':

  (define (map f l)
    (if (null? l)
      null
      (cons (f (first l)) (map f (rest l)))))

The first thing in turning it into a `map/k' is adding a `k' input,

  (define (map f l k)
    (if (null? l)
      null
      (cons (f (first l)) (map f (rest l)))))

and now we need to face the fact that the `f' input is itself one with a
continuation -- an `f/k':

  (define (map/k f/k l k)
    (if (null? l)
      null
      (cons (f (first l)) (map f (rest l)))))

Consider now the single `f' call -- that should turn into a call to
`f/k' with some continuation:

  (define (map/k f/k l k)
    (if (null? l)
      null
      (cons (f/k (first l) ???) (map f (rest l)))))

but since `f/k' will involve a web interaction, it will lead to killing
the `cons' around it.  The solution is to move that `cons' into the
continuation that is handed to `f/k' -- and as usual, this involves the
second `cons' argument -- the continuation is derived from replacing the
`f/k' call by a hole:

      (cons <*> (map f (rest l)))

and abstracting that hole, we get:

  (define (map/k f/k l k)
    (if (null? l)
      null
      (f/k (first l)
           (lambda (result)
             (cons result (map f (rest l)))))))

We now do exactly the same for the recursive `map' call -- it should use
`map/k' with `f/k' and some continuation:

  (define (map/k f/k l k)
    (if (null? l)
      null
      (f/k (first l)
           (lambda (result)
             (cons result (map/k f/k (rest l) ???))))))

and we need to move the surrounding `cons' yet again into this
continuation.  The holed expression is:

             (cons result <*>)

and abstracting that and moving it into the `map/k' continuation we get:

  (define (map/k f/k l k)
    (if (null? l)
      null
      (f/k (first l)
           (lambda (result)
             (map/k f/k (rest l)
                    (lambda (new-rest)
                      (cons result new-rest)))))))

There are just one more problem with this -- the `k' argument is never
used.  This implies two changes, since it needs to be used once in each
of the conditional branches.  Can you see where it should be added?
(Try to do this before reading the code below.)

The complete code follows:

  ----------------------------------------------------------------------
  (define (map/k f/k l k)
    (if (null? l)
      (k null)
      (f/k (first l)
           (lambda (result)
             (map/k f/k (rest l)
                    (lambda (new-rest)
                      (k (cons result new-rest))))))))
  (define (sum l) (foldl + 0 l))
  (define (square n) (* n n))
  (define (read-number/k prompt k)
    (web-read/k (format "~a number" prompt) k))
  (define (read-squared/k prompt k)
    (read-number/k prompt (lambda (n) (k (square n)))))
  (map/k read-squared/k
         '("First" "Second" "Third")
         (lambda (l) (web-display (sum l))))
  ----------------------------------------------------------------------

========================================================================
>>> Highlevel Overview on Continuations

Very roughly speaking, the transformation we made turns a function call
like

  (...stuff... (f ...args...) ...more-stuff...)

into

  (f/k ...args...
       (lambda (<*>)
         (...stuff... <*> ...more-stuff...)))

This is the essence of the solution to the statelessness problem: to
remember where we left off, we conveniently flip the expression
inside-out so that its context is all stored in its continuation.  One
thing to note is that we did this only for functions that had some kind
of web interaction, either directly or indirectly (since in the indirect
case they still need to carry around the continuation).

If we wanted to make this process a completely mechanical one, then we
wouldn't have been able to make this distinction.  After all, a function
like `map' is perfectly fine as it is, unless it happens to be used with
a continuation-carrying function -- and that's something that we know
only at runtime.  We would therefore need to transform *all* function
calls as above, which in turn means that all functions would need to get
an extra continuation argument.

Here are a few things to note about such fully-transformed code:

* All function calls in such code are tail calls.  There is no single
  call with some context around it that is left for the time when the
  call is done.  This is the exact property that makes it useful for a
  stateless interaction: such contexts are bad since a web interaction
  will mean that the context is discarded and lost.  (In our pretend
  system, this is done by throwing an error.)  Having no non-tail
  context means that capturing the continuation argument is sufficient,
  and no context gets lost.

* An implication of this, when you consider how the language is
  implemented, is that there is no need to have anything on "the stack"
  to execute fully transformed code.  (If you'd use the stepper on such
  code, there would be no accumulation of context.)  So is this some
  radical new way of computing without a stack?  Not really: if you
  think about it, continuation arguments hold the exact same information
  that is traditionally put on the stack.  (There is therefore an
  interesting relationship between continuations and runtime stacks, and
  in fact, one way of making it possible to grab continuations without
  doing such a transformation is to capture the current stack.)

* The evaluation order is fixed.  Obviously, if Racket guarantees a
  left-to-right evaluation, then the order is always fixed -- but in the
  fully transformed code there are no function calls where this makes
  any difference.  If Racket were to change, the transformed code would
  still retain the order it uses.  More specifically, when we do the
  transformation, we control the order of evaluation by choosing how to
  proceed at every point.  But there's more: the resulting code is
  independent of the evaluation strategy of the language.  Even if the
  language is lazy, the transformed code is still executing things in
  the same order.  (Alternatively, we could convert things so that the
  resulting computation corresponds to a lazy evaluation strategy even
  in a strict language.)

* In other words, the converted code is completely sequential.  The
  conversion process requires choosing left-to-right or delaying some
  evaluations (or all), but the resulting code is free from any of these
  and has exactly one specific (sequential) order.  You can therefore
  see how this kind of transformation is something that a compiler would
  want to do, since the resulting sequential code is easier for
  execution on a sequential base (like machine code, or C code).
  Another way to see this is that we have explicit names for each and
  every intermediate result -- so the converted code would have a direct
  mapping between identifiers and machine registers (unlike "plain" code
  where some of these are implicit and compilation needs to make up
  names).

* The transformation is a *global* one.  Not only do we have to
  transform the first top-level expression that makes up the web
  application, we also need to convert every function that is mentioned
  in the code, and in functions that those functions mentioned, etc.
  Even worse, the converted code is very different from the original
  version, since everything is shuffled around -- in a way that matches
  the sequential execution, but it's very hard to even see the original
  intention through all of these explicit continuations and the new
  intermediate result names.

  The upshot of this is that it's not really something that we need to
  do manually, but instead we'd like it to be done automatically for us,
  by the compiler of the language.

What we did here is the tedious way of getting continuations: we
basically implemented them by massaging our code, turning it inside-out
into code with the right shape.  The problem with this is that the
resulting code is no longer similar to what we had originally written,
which makes it more difficult to debug and to maintain.  We therefore
would like to have this done in some automatic way, ideally in a way
that means that we can leave our plain original code as is.

========================================================================
>>> Implementing an Automatic Continuation Converter

[[[ PLAI Chapter 18 ]]]

The converted code that we produced manually above is said to be written
in "Continuation Passing Style", or CPS.  What we're looking for is for
a way to generate such code automatically -- a way to "CPS" a given
source code.  When you think about it, this process is essentially a
source to source function which should be bolted onto the compiler or
evaluator.  In fact, if we want to do this in Racket, then this
description makes it sound a lot like a macro -- and indeed it could be
implemented as such.

[Note that "CPS" has two related but distinct meanings here: you could
have code that is written "in CPS style", which means that it handles
explicit continuations.  Uses of this term usually refer to using
continuation functions in some places in the code, not for fully
transformed code.  The other meaning is used for fully-CPS-ed code,
which is almost never written directly.  In addition, "CPS" is often
used as a verb -- either the manual process of refactoring code into
passing some continuations explicitly (in the first case), or the
automatic process of fully converting code (in the second one).]

Before we get to the actual implementation, consider how we did the
translation -- there was a difference in how we handled plain top-level
expressions and library functions.  In addition, we had some more
discounts in the manual process -- one such discount was that we didn't
treat all value expressions as possible computations that require
conversion.  For example, in a function application, we took the
function sub-expression as a simple value and left it as is, but for an
automatic translation we need to convert that expression too since it
might itself be a more complicated expression.

Instead of these special cases and shortcuts, we'll do something more
uniform: we will translate *every* expression into a function.  This
function will accept a receiver (= a continuation) and will pass it the
value of the expression.  This will be done for *all* expressions, even
simple ones like plain numbers, for example, we will translate the `5'
expression into (lambda (k) (k 5)), and the same goes for other
constants and plain identifiers.  Since we're specifying a
transformation here, we will treat it as a kind of a meta function and
use a `CPS[x]' to make it easy to talk about:

  CPS[5]
  -->
  (lambda (k) (k 5)) ; same for other numbers and constants

  CPS[x]
  -->
  (lambda (k) (k x)) ; same for other identifiers

When we convert a primitive function application, we still do the usual
thing, which is now easier to see as a general rule -- using `CPS[?]' as
the meta function that does the transformation:

  CPS[(+ E1 E2)]
  -->
  (lambda (k)        ; everything turns to cont.-consuming functions
    (CPS[E1]         ; the CPS of E1 -- it expects a cont. argument
     (lambda (v1)    ; we send this cont to CPS[E1], so v1 is its value
       (CPS[E2]      ; same for E2 -- expects a cont.
        (lambda (v2) ; and again, v2 becomes the value of E2
          (k (+ v1 v2))))))) ; finally return the sum to our own cont.

In the above, you can see that (CPS[E] (lambda (v) ...)) can be read as
"evaluate `E' and bind the result to `v'".  (But note that the CPS
conversion is not doing any evaluation, it just reorders code to
determine how it gets evaluated when it later runs -- so "compute" might
be a better term to use here.)  With this in mind, we can deal with
other function applications: evaluate the function form, evaluate the
argument form, then apply the first value on the second value, and
finally wrap everything with a (lambda (k) ...) and return the result to
this continuation:

  CPS[(E1 E2)]
  -->
  (lambda (k)
    (CPS[E1]         ; bind the result of evaluating E1
     (lambda (v1)    ; to v1
       (CPS[E2]      ; and the result of evaluating E2
        (lambda (v2) ; to v2
          (k (v1 v2))))))) ; apply and return the result

But this is the rule that we should use for primitive non-continuation
functions only -- it's similar to what we did with `+' (except that we
skipped evaluating `+' since it's known).  Instead, we're dealing here
with functions that are defined in the "web language" (in the code that
is being converted), and as we've seen, these functions get a `k'
argument which they use to return the result to.  That was the whole
point: pass `k' on to functions, and have them return the value directly
to the `k' context.  So the last part of the above should be fixed:

  CPS[(E1 E2)]
  -->
  (lambda (k)
    (CPS[E1]         ; bind the result of evaluating E1
     (lambda (v1)    ; to v1
       (CPS[E2]      ; and the result of evaluating E2
        (lambda (v2) ; to v2
          (v1 v2 k)))))) ; apply and have it return the result to k

There's a flip side to this transformation -- whenever a function is
created with a `lambda' form, we need to add a `k' argument to it, and
make it return its value to it.  Then, we need to "lift" the whole
function as usual, using the same transformation we used for other
values in the above.  We'll use `k' for the latter continuation
argument, and `cont' for the former:

  CPS[(lambda (arg) E)]
  -->
  (lambda (k)        ; this is the usual
    (k               ; lifting of values
     (lambda (arg cont) ; the translated function has a cont. input
       (CPS[E] cont)))) ; the translated body returns its result to it

It is interesting to note the two continuations in the translated
result: the first one (using `k') is the continuation for the function
value, and the second one (using `cont') is the continuation used when
the function is applied.  Comparing this to our evaluators -- we can say
that the first roughly corresponds to evaluating a function form to get
a closure, and the second corresponds to evaluating the body of a
function when it's applied, which means that `cont' is the dynamic
continuation that matches the dynamic context in which the function is
executed.  Inspecting the CPS-ed form of the identity function is
unsurprising: it simply passes its first argument (the "real" one) into
the continuation since that's how we return values in this system:

  CPS[(lambda (x) x)]
  -->
  (lambda (k)
    (k
     (lambda (x cont)
       (CPS[x] cont))))
  -->
  (lambda (k)
    (k
     (lambda (x cont)
       ((lambda (k) (k x)) cont)))) ; redundant function application
  -->
  (lambda (k)
    (k
     (lambda (x cont)
       (cont x))))

Note the reduction of a trivial application -- doing this systematic
conversion leads to many of them.

We now get to the transformation of the form that is the main reason we
started with all of this -- `web-read'.  This transformation is simple,
it just passes along the continuation to `web-read/k':

  CPS[(web-read E)]
  -->
  (lambda (k)
    (CPS[E]          ; evaluate the prompt expression
     (lambda (v)     ; and bind it to v
       (web-read/k v k)))) ; use the prompt value and the current cont.

We also need to deal with `web-display' -- we changed the function
calling protocol by adding a continuation argument, but `web-display' is
defined outside of the CPS-ed language so it doesn't have that argument.
Another way of fixing it could be to move its definition into the
language, but then we'll still need to have a special treatment for
the `error' that it uses.

  CPS[(web-display E)]
  -->
  (lambda (k)
    (CPS[E]          ; evaluate the expression
     (lambda (v)     ; and bind it to v
       (web-display v))))

As you can see, all of these transformations are simple rewrites.  We
can use a simple `syntax-rules' macro to implement this transformation,
essentially creating a DSL by translating code into plain Racket.  Note
that in the specification above we've implicitly used some parts of the
input as keywords -- `lambda', `+', `web-read', and `define' -- this is
reflected in the macro code.  The order of the rules is important, for
example, we need to match first on (web-read E) and then on the more
generic (E1 E2), and we ensure that the last default lifting of values
has a simple expression by matching on (x ...) before that.

  (define-syntax CPS
    (syntax-rules (+ lambda web-read web-display) ; <-- keywords
      [(CPS (+ E1 E2))
       (lambda (k)
         ((CPS E1)
          (lambda (v1)
            ((CPS E2)
             (lambda (v2)
               (k (+ v1 v2)))))))]
      [(CPS (web-read E))
       (lambda (k)
         ((CPS E)
          (lambda (v)
            (web-read/k v k))))]
      [(CPS (web-display E))
       (lambda (k)
         ((CPS E)
          (lambda (v)
            (web-display v))))]
      [(CPS (E1 E2))
       (lambda (k)
         ((CPS E1)
          (lambda (v1)
            ((CPS E2)
             (lambda (v2)
               (v1 v2 k))))))]
      [(CPS (lambda (arg) E))
       (lambda (k)
         (k (lambda (arg cont)
              ((CPS E)
               cont))))]
      ;; the following pattern ensures that the last rule is used only
      ;; with simple values and identifiers
      [(CPS (x ...))
       ---syntax-error---]
      [(CPS V)
       (lambda (k)
         (k V))]))

The transformation that this code implements is one of the oldest CPS
transformations -- it is called the Fischer Call by Value CPS
transformation, and is due Michael Fischer.  There has been much more
research into such transformations -- the Fischer translation, while
easy to understand due to its uniformity, introduces significant
overhead in the form of many new functions in its result.  Some of these
are easy to optimize -- for example, things like ((lambda (k) (k v)) E)
could be optimized to just (E v) assuming a left-to-right evaluation
order or proving that E has no side-effects (and Racket performs this
optimization and several others), but some of the overhead is not easily
optimized.  There have been several other CPS transformations, in an
attempt to avoid such overhead.

Finally, trying to run code using this macro can be a little awkward.
We need to explicitly wrap all values in definitions by a `CPS', and we
need to invoke top-level expressions with a particular continuation --
`web-display' in our context.  We can do all of that with a convenience
macro that will transform a number of definitions followed by an
optional expression.

[Note the use of `begin' -- usually, it is intended for sequential
execution, but it is also used as macro results when we need a macro to
produce multiple expressions (since the result of a macro must be a
single S-expression) -- this is why it's used here.]

  (define-syntax CPS-code
    (syntax-rules (define)
      [(CPS-code (define (id arg) E) more ...)
       (begin (define id ((CPS (lambda (arg) E)) (lambda (v) v)))
              (CPS-code more ...))]
      [(CPS-code (define id E) more ...)
       (begin (define id ((CPS E) (lambda (v) v)))
              (CPS-code more ...))]
      [(CPS-code last-expr)
       ((CPS last-expr) web-display)]
      [(CPS-code) ; happens when there is no non-definitions at the end
       (begin)])) ; so do nothing in this case

The interesting thing that this macro does is arrange to have a proper
continuation for definitions and top-level expressions.  In the latter
case, it passes `web-display' as the continuation, and in the former
case, it passes the identity function as the continuation -- which is
used to "lower" the lifted value from its continuation form into a plain
value.  Using the identity function as a continuation is not really
correct: it means that if evaluating the expression to be bound performs
some web interaction, then the definition will be aborted, leaving the
identifier unbound.  The way to solve this is by arranging for the
definition operation to be done in the continuation, for example, we can
get closer to this using an explicit mutation step:

      [(CPS-code (define id E) more ...)
       (begin (define id #f)
              ((CPS E) (lambda (v) (set! id v)))
              (CPS-code more ...))]

But there are two main problems with this: first, the rest of the code
-- (CPS-code more ...) -- should also be done in the continuation, which
will defeat the global definitions.  We could try to use the contiuation
to get the scope:

      [(CPS-code (define id E) more ...)
       ((CPS E) (lambda (id) (CPS-code more ...)))]

but that breaks recursive definitions.  In any case, the second problem
is that this is not accurate even if we solved this problem: we really
need to have parts of the Racket definition mechanism exposed to make it
work.  So we settle with the above simple version as an approximation.

For reference, the complete code at this point follows.

  ---<<<SIMPLE-WEB-LANGUAGE>>>------------------------------------------
  ;; Simulation of web interactions with a CPS converter (not an
  ;; interpreter)

  #lang racket

  (define (nothing-to-do)
    (error "There is no computation in progress now."))

  (define what-next (box nothing-to-do))

  (define (web-display n)
    (set-box! what-next nothing-to-do)
    (error 'web-output "~s" n))

  (define (web-read/k prompt k)
    (set-box! what-next
              (lambda ()
                (printf "~a: " prompt)
                (k (read))))
    (error 'web-read/k "enter (resume) to continue"))

  (define (resume)
    ;; to avoid mistakes, we clear out `what-next' before invoking it
    (let ([next (unbox what-next)])
      (set-box! what-next nothing-to-do)
      (next)))

  (define-syntax CPS
    (syntax-rules (+ lambda web-read web-display) ; <-- keywords
      [(CPS (+ E1 E2))
       (lambda (k)
         ((CPS E1)
          (lambda (v1)
            ((CPS E2)
             (lambda (v2)
               (k (+ v1 v2)))))))]
      [(CPS (web-read E))
       (lambda (k)
         ((CPS E)
          (lambda (v)
            (web-read/k v k))))]
      [(CPS (web-display E))
       (lambda (k)
         ((CPS E)
          (lambda (v)
            (web-display v))))]
      [(CPS (E1 E2))
       (lambda (k)
         ((CPS E1)
          (lambda (v1)
            ((CPS E2)
             (lambda (v2)
               (v1 v2 k))))))]
      [(CPS (lambda (arg) E))
       (lambda (k)
         (k (lambda (arg cont)
              ((CPS E)
               cont))))]
      ;; the following pattern ensures that the last rule is used only
      ;; with simple values and identifiers
      [(CPS (x ...))
       ---syntax-error---]
      [(CPS V)
       (lambda (k)
         (k V))]))

  (define-syntax CPS-code
    (syntax-rules (define)
      [(CPS-code (define (id arg) E) more ...)
       (begin (define id ((CPS (lambda (arg) E)) (lambda (v) v)))
              (CPS-code more ...))]
      [(CPS-code (define id E) more ...)
       (begin (define id ((CPS E) (lambda (v) v)))
              (CPS-code more ...))]
      [(CPS-code last-expr)
       ((CPS last-expr) web-display)]
      [(CPS-code) ; happens when there is no non-definitions at the end
       (begin)])) ; so do nothing in this case
  ----------------------------------------------------------------------

Here is a quick example of using this:

  (CPS-code
    (web-display (+ (web-read "First number")
                    (web-read "Second number"))))

Note that this code uses `web-display', which is not really needed since
`CPS-code' would use it as the top-level continuation.  (Can you see why
it works the same either way?)

A slightly more complicated example:

  (CPS-code
    (define (add n)
      (lambda (m)
        (+ m n)))
    (define (read-and-add n)
      ((add n) (web-read "Another number")))
    (read-and-add (web-read "A number")))

Using this for the other examples is not possible with the current state
of the translation macro.  These example will require extending the CPS
transformation with functions of any arity, multiple expressions in a
body, and it recognize additional primitive functions.  None of these is
difficult, it will just make it more verbose.

========================================================================
>>> Continuations as a Language Feature

[[[ conceptually, this is between PLAI Chapters 18 and 19 ]]]

In the list of CPS transformation rules there were two rules that
deserve additional attention in how they deal with their continuation.

First, note the rule for `web-display':

      [(CPS (web-display E))
       (lambda (k)
         ((CPS E)
          (lambda (v)
            (web-display v))))]

-- it simply ignores its continuation.  This means that whenever
`web-display' is used, the rest of the computation is simply discarded,
which seems wrong -- it's the kind of problem that we've encountered
several times when we discussed the transforming web application code.
Of course, this doesn't matter much for our implementation of
`web-display' since it aborts the computation anyway using `error' --
but what if we did that intentionally?  We would get a kind of an "abort
now" construct: we can implement this as a new `abort' form that does
just that:

  (define-syntax CPS
    (syntax-rules (...same... abort) ; <-- new keyword
      ...
      [(CPS (abort E))
       (lambda (k) ((CPS E) (lambda (v) v)))] ; ignore `k'
      ...))

You could try that -- (CPS-code (+ 1 2)) produces 3 as "web output", but
(CPS-code (+ 1 (abort 2))) simply returns 2.  In fact, it doesn't matter
how complicated the code is -- as soon as it encounters an `abort' the
whole computation is discarded and we immediately get its result, for
example, try this:

  (CPS-code
    (define (add n)
      (lambda (m)
        (+ m n)))
    (define (read-and-add n)
      ((abort 999) ((add n) (web-read "Another number"))))
    (read-and-add (web-read "A number")))

it reads the first number and then it immediately returns 999.  This
seems like a potentially useful feature, except that it's a little too
"untamed" -- it aborts the program completely, getting all the way back
to the top-level with a result.  (It's actually quite similar to
throwing an exception, without a way to catch it.)  It would be more
useful to somehow control the part of the computation that gets aborted
instead.

That leads to the second exceptional form in our translator: `web-read'.
If you look closely at all of our transformation rules, you'll see that
the continuation argument is never made accessible to user code -- the
`k' argument is always generated by the macro (and inaccessible to user
code due to the hygienic macro system).  The continuation is only passed
as the extra argument to user functions, but in the rule that adds this
argument:

      [(CPS (lambda (arg) E))
       (lambda (k)
         (k (lambda (arg cont)
              ((CPS E)
               cont))))]

the new `cont' argument is introduced by the macro so it is inaccessible
as well.  The only place where the `k' argument is actually used is in
the `web-read' rule, where it is sent to the resulting `web-read/k'
call.  (This makes sense, since web reading is how we mimic web
interaction, and therefore it is the only reason for CPS-ing our code.)
However, in our fake web framework this function is a given built-in, so
the continuation is still not accessible for user code.  But what if we
pass the continuation argument to a user function in a way that
(intentionally) exposes it?  We can achieve this by writing a function
that is similar to `web-read/k', except that it will somehow pass the
continuation to user code.  A simple way to do that is to have the new
function take a function value as its primary input, and call this
function with the continuation (which is still received as the implicit
second argument):

  (define (call-k f k)
    (f k))

This is close, but it fails because it doesn't follow our translated
function calling protocol, where every function receives two inputs --
the original argument, and the continuation.  Because of this, we need
to call `f' with a second continuation value, which is `k' as well:

  (define (call-k f k)
    (f k k))

But we also fail to follow the calling protocol by passing `k' as is: it
is a continuation value, which in our CPS system is a one-argument
function.  In fact, this is another indication that continuations are
not accessible to user code -- they don't follow the same function
calling protocol.  In fact, it is best to think about continuations as
*meta* values that are not part of the user language -- yet.  To make it
usable, we need to wrap it so we get the usual two-argument function
which user code can call:

  (define (call-k f k)
    (f (lambda (val cont) (k val)) k))

This explicit wrapping is related to the fact that continuations are a
kind of meta-level value -- and the wrapping is needed to "lower" it to
the user's world.  (This is sometimes called "reification": a meta value
is *reified* as a user value.)

Using this new definition, we can write code that can access its own
continuation as a plain value.  Here is a simple example that grabs the
top-level continuation and labels it `abort', then uses it in the same
way we've used the above `abort':

  > (CPS-code (call-k (lambda (abort) (+ 1 (abort 2)))))
  web-output: 2

But we can grab any continuation we want, not just the top-level one:

  (CPS-code (+ 100 (call-k (lambda (abort) (+ 1 (abort 2))))))
  web-output: 102

Side note: how come we didn't need a new CPS translation rule for this
function?  There is no need for one, since `call-k' is already written
in a way that follows our calling convention, and no translation rule is
needed.  In fact, no such rule is needed for `web-read' too -- except
for changing the call to `web-read/k', it does exactly the same thing
that a function call does, so we can simply rename `web-read/k' as
`web-read' and drop the rule.  (Note that the rewritten function call
will have a (CPS web-read) -- but CPS-ing an identifier results in the
identifier itself.)  The same holds for `web-display' -- we just need to
make it adhere to the calling convention and add a `k' input which is
ignored.  One minor complication is that `web-display' is also used as a
continuation value for a top-level expression in `CPS-code' -- so we
need to wrap it there.

The resulting code follows:

  ----------------------------------------------------------------------
  ;; Simulation of web interactions with a CPS converter (not an
  ;; interpreter)

  #lang racket

  (define (nothing-to-do)
    (error "There is no computation in progress now."))

  (define what-next (box nothing-to-do))

  (define (web-display n k)
    (set-box! what-next nothing-to-do)
    (error 'web-output "~s" n))

  (define (web-read prompt k)
    (set-box! what-next
              (lambda ()
                (printf "~a: " prompt)
                (k (read))))
    (error 'web-read "enter (resume) to continue"))

  (define (resume)
    ;; to avoid mistakes, we clear out `what-next' before invoking it
    (let ([next (unbox what-next)])
      (set-box! what-next nothing-to-do)
      (next)))

  (define (call-k f k)
    (f (lambda (val cont) (k val)) k))

  (define-syntax CPS
    (syntax-rules (+ lambda)
      [(CPS (+ E1 E2))
       (lambda (k)
         ((CPS E1)
          (lambda (v1)
            ((CPS E2)
             (lambda (v2)
               (k (+ v1 v2)))))))]
      [(CPS (E1 E2))
       (lambda (k)
         ((CPS E1)
          (lambda (v1)
            ((CPS E2)
             (lambda (v2)
               (v1 v2 k))))))]
      [(CPS (lambda (arg) E))
       (lambda (k)
         (k (lambda (arg cont)
              ((CPS E)
               cont))))]
      ;; the following pattern ensures that the last rule is used only
      ;; with simple values and identifiers
      [(CPS (x ...))
       ---syntax-error---]
      [(CPS V)
       (lambda (k)
         (k V))]))

  (define-syntax CPS-code
    (syntax-rules (define)
      [(CPS-code (define (id arg) E) more ...)
       (begin (define id ((CPS (lambda (arg) E)) (lambda (v) v)))
              (CPS-code more ...))]
      [(CPS-code (define id E) more ...)
       (begin (define id ((CPS E) (lambda (v) v)))
              (CPS-code more ...))]
      [(CPS-code last-expr)
       ((CPS last-expr) (lambda (v) (web-display v 'whatever)))]
      [(CPS-code) ; happens when there is no non-definitions at the end
       (begin)])) ; so do nothing in this case
  ----------------------------------------------------------------------

Obviously, given `call-k' we could implement `web-read/k' in user code:
`call-k' makes the current continuation available and going on from
there is simple (it will require a little richer language, so we will do
that in a minute).  In fact, there is no real reason to stick to the
fake web framework to play with continuations.  (Note: since we don't
throw an error to display the results, we can also allow multiple
non-definition expressions in `CPS-code'.)

  ---<<<CPS-LANGUAGE>>>-------------------------------------------------
  ;; A language that is CPS-transformed (not an interpreter)

  #lang racket

  (define (call-k f k)
    (f (lambda (val cont) (k val)) k))

  (define-syntax CPS
    (syntax-rules (+ lambda)
      [(CPS (+ E1 E2))
       (lambda (k)
         ((CPS E1)
          (lambda (v1)
            ((CPS E2)
             (lambda (v2)
               (k (+ v1 v2)))))))]
      [(CPS (E1 E2))
       (lambda (k)
         ((CPS E1)
          (lambda (v1)
            ((CPS E2)
             (lambda (v2)
               (v1 v2 k))))))]
      [(CPS (lambda (arg) E))
       (lambda (k)
         (k (lambda (arg cont)
              ((CPS E)
               cont))))]
      ;; the following pattern ensures that the last rule is used only
      ;; with simple values and identifiers
      [(CPS (x ...))
       ---syntax-error---]
      [(CPS V)
       (lambda (k)
         (k V))]))

  (define-syntax CPS-code
    (syntax-rules (define)
      [(CPS-code (define (id arg) E) more ...)
       (begin (define id ((CPS (lambda (arg) E)) (lambda (v) v)))
              (CPS-code more ...))]
      [(CPS-code (define id E) more ...)
       (begin (define id ((CPS E) (lambda (v) v)))
              (CPS-code more ...))]
      [(CPS-code expr more ...)
       (begin ((CPS expr) (lambda (v) v))
              (CPS-code more ...))]
      [(CPS-code) (begin)])) ; done

  (CPS-code (call-k (lambda (abort) (+ 1 (abort 2))))
            (+ 100 (call-k (lambda (abort) (+ 1 (abort 2))))))
  ----------------------------------------------------------------------

========================================================================
>>> Continuations in Racket

As we have seen, CPS-ing code makes it possible to implement web
applications with a convenient interface.  This is fine in theory, but
in practice it suffers from some problems.  Some of these problems are
technicalities: it relies on proper implementation of tail calls (since
all calls are tail calls, and no context), and it represents the
computation stack as a chain of closures and therefore prevents the
usual optimizations.  But there is one problem that is much more
serious: it is a *global* transformation, and as such, it requires
access to the complete program code.  As an example, consider how
`CPS-code' deals with definitions: it uses an identity function as the
continuation, but that wasn't the proper way to do them, since it would
break if computing the value performs some web interaction.  A good
solution would instead put the side-effect that `define' performs in the
continuation -- but this side effect is not even available for us when
we work inside Racket.

Because of this, the proper way to make continuations available is for
the language implementation itself to provide it.  There are a few
languages that do just that -- and Scheme has pioneered this as part of
the core requirements that the standard dictates: a Scheme
implementation needs to provide `call-with-current-continuation', which
is essentially identical to our `call-k'.  Usually it is also provided
with a shorter name, `call/cc'.  Here are our two examples, re-done with
Racket's built-in `call/cc':

  (call/cc (lambda (abort) (+ 1 (abort 2))))
  (+ 100 (call/cc (lambda (abort) (+ 1 (abort 2)))))

[Side note: continuations as we see here are still provided only by a
few functional languages.  However, they are slowly making their way
into more mainstream languages -- Ruby has these continuations too, and
several other languages provide more limited variations, like generators
in Python.  On the other hand, Racket provides a much richer
functionality: it has delimited continuations (which represents only a
part of a computation context), and its continuations are also
composable -- a property that goes beyond what we see here.]

Racket also provides a more convenient `let/cc' form, which exposes the
"grab the current continuation" pattern more succinctly:

  (define-syntax-rule (let/cc k body ...)
    (call/cc (lambda (k) body ...)))

and the two examples become:

  (let/cc abort (+ 1 (abort 2)))
  (+ 100 (let/cc abort (+ 1 (abort 2))))

When it gets to choosing an implementation strategy, there are two
common approaches: one is to do the CPS transformation at the compiler
level, and another is to capture the actual runtime stack and wrap it in
an applicable continuation objects.  The former can lead to very
efficient compilation of continuation-heavy programs, but the latter
makes it easier to deal with foreign functions (consider higher order
functions that are given as a library where you don't have its source)
and allows using the normal runtime stack that CPUs are using very
efficiently.  Racket implements continuations with the latter approach
mainly for these reasons.

To see how these continuations expose some of the implementation details
that we normally don't have access to, consider grabbing the
continuation of a definition expression:

  > (define b (box #f))
  > (define a (let/cc k (set-box! b k) 123))
  > a
  123
  > ((unbox b) 1000)
  > a
  1000

[Note that using a top-level (let/cc abort ...code...) is not really
aborting for a reason that is related to this: a true `abort' must
capture the continuation before any computation begins.  A natural place
to do this is in the REPL implementation.]

Finally, we can use these to re-implement our fake web framework, using
Racket's continuations instead of performing our own transformation.
The only thing that requires continuations is our `web-read' -- and
using the Racket facilities we can implement it as follows:

  (define (web-read prompt) ; no `k' input
    (let/cc k ; instead, get it with `let/cc'
      ;; and the body is the same as it was
      (set-box! what-next
                (lambda ()
                  (printf "~a: " prompt)
                  (k (read))))
      (error 'web-read "enter (resume) to continue")))

Note that this kind of an implementation is no longer a "language" -- it
is implemented as a plain library now, demonstrating the flexibility
that having continuations in our language enables.  While this is still
just our fake toy framework, it is the core way in which the Racket web
server is implemented (see the "addition server" implementation above),
using a hash table that maps freshly made URLs to stored continuations.
The complete code follows:

  ---<<<CONTINUATION-BASED-WEB-LIBRARY>>>-------------------------------
  ;; Simulation of web interactions with Racket's built-in continuation
  ;; facility.

  #lang racket

  (define (nothing-to-do)
    (error "There is no computation in progress now."))

  (define what-next (box nothing-to-do))

  (define (web-display n)
    (set-box! what-next nothing-to-do)
    (error 'web-output "~s" n))

  (define (web-read prompt)
    (let/cc k
      (set-box! what-next
                (lambda ()
                  (printf "~a: " prompt)
                  (k (read))))
      (error 'web-read "enter (resume) to continue")))

  (define (resume)
    ;; to avoid mistakes, we clear out `what-next' before invoking it
    (let ([next (unbox what-next)])
      (set-box! what-next nothing-to-do)
      (next)))
  ----------------------------------------------------------------------

Using this, you can try out some of the earlier examples, which now
become much simpler since there is no need to do any CPS-ing.  For
example, here is the example that required transforming `map/k' -- it
now uses the plain Racket `map' function.  In fact, that's the exact
code we started that example with:

  (define (sum l) (foldl + 0 l))
  (define (square n) (* n n))
  (define (read-number prompt)
    (web-read (format "~a number" prompt)))
  (web-display (sum (map (lambda (prompt) (square (read-number prompt)))
                         '("First" "Second" "Third"))))

Note how `web-read' is executed directly -- it is a plain library
function.

========================================================================
>>> Playing with Continuations

[[[ PLAI Chapter 19 ]]]

So far we've seen a number of "tricks" that can be done with
continuations.  The simplest was aborting a computation -- here's an
implementation of functions with a `return' that can be used to exit the
function early:

  (define-syntax (fun stx)
    (syntax-case stx ()
      [(_ name (x ...) body ...)
       (with-syntax ([return (datum->syntax #'name 'return)])
         #'(define (name x ...) (let/cc return body ...)))]))

  ;; try it:
  (fun mult (list)
    (define (loop list)
      (cond [(null? list) acc]
            [(zero? (first list)) (return 0)] ; early return
            [else (* (first list) (loop (rest list)))]))
    (loop list))
  (mult '(1 2 3 0 x))

[Side note: This is a cheap demonstration.  If we rewrite the loop
tail-recursively, then aborting it is simple -- just return 0 instead of
continuing the loop.  And that's not a coincidence, aborting from a
tail-calling loop is easy, and CPS makes such aborts possible by making
only tail calls.]

But such uses of continuations are simple because they're used only to
"jump out" of some (dynamic) context.  More exotic uses of continuations
rely on the ability to jump into a previously captured continuation.  In
fact, our `web-read' implementation does just that (and more).  The main
difference is that in the former case the continuation is used exactly
once -- either explicitly by using it, or implicitly by returning a
value (without aborting).  If a continuation can be used after the
corresponding computation is over, then why not use it over and over
again...  For example, we can try an infinite loop by capturing a
continuation and later use it as a jump target:

  (define (foo)
    (define loop (let/cc k k))    ; captured only for the context
    (printf "Meh.\n")
    (loop 'something))            ; need to pass in something...

This almost works -- we get two printouts so clearly the jump was
successful.  The problem is that the captured `loop' continuation is the
one that expects a value to bind to `loop' itself, so the second
attempted call has 'something as the value of `loop', obviously, leading
to an error.  This can be used as a hint, for a solution -- simply pass
the continuation to itself:

  (define (foo)
    (define loop (let/cc k k))
    (printf "Meh.\n")
    (loop loop))                  ; keep the value of `loop'

Another way around this problem is to capture the continuation that is
just *after* the binding -- but we can't do that (try it...).  Instead,
we can use side-effects:

  (define (foo)
    (define loop (box #f))
    (let/cc k (set-box! loop k))  ; cont. of the outer expression
    (printf "Meh.\n")
    ((unbox loop) 'something))

[Note: the 'something value goes to the continuation which makes it the
result of the (let/cc ...) expression -- which means that it's never
actually used now.]

This might seem like a solution that is not as "clean", since it uses
mutation -- but note that the problem that we're solving stems from a
continuation that exposes the mutation that Racket performs when it
creates a new binding.

Here's an example of a loop that does something a little more
interesting in a goto-like kind of way:

  (define (foo)
    (define n (box 0))
    (define loop (box #f))
    (let/cc k (set-box! loop k))
    (set-box! n (add1 (unbox n)))
    (printf "n = ~s\n" (unbox n))
    ((unbox loop)))

[Note: in this example the continuation is called *without* any inputs.
How is this possible?  As we've seen, the 'something value in the last
example is the never-used result of the `let/cc' expression.  In this
case, the continuation is called with no input, which means that the
`let/cc' expression evaluates to ... nothing!  This is not just some
`void' value, but no value at all.  The complete story is that in Racket
expressions can evaluate to *multiple* values, and in this case, it's
zero.  In any case, we can ignore this, since those zero values are
unused.]

Given such examples it's no wonder that continuations tend to have a
reputation for being "similar to goto in their power".  This reputation
has some vague highlevel justification in that both features can produce
obscure "spaghetti code" -- but in practice they are very different.  On
one hand continuations are more limited: unlike `goto', you can only
jump to a continuation that you have already "visited".  On the other
hand, jumping to a continuation is doing much more than jumping to a
goto label, the latter changes the next instruction to execute (the
"program counter" register), but the former changes current computation
context (in low level terms, the PC and the stack).

To demonstrate how different continuation are from plain gotos, we'll
start with a variation of the above loop -- instead of performing the
loop we just store it in a global box, and we return the counter value
instead of printing it:

  (define loop (box #f))

  (define (foo)
    (define n (box 0))
    (let/cc k (set-box! loop k))
    (set-box! n (add1 (unbox n)))
    (unbox n))

Now, the first time we call (foo), we get 1 as expected, and then we can
call (unbox loop) to re-invoke the continuation and get the following
numbers:

  > (foo)
  1
  > ((unbox loop))
  2
  > ((unbox loop))
  3

[Interesting experiment: try doing the same, but use (list (foo)) as the
first interaction, and the same ((unbox loop)) later.]

The difference between this use and a `goto' is now evident: we're not
just just jumping to a label -- we're jumping back into a computation
that returns the next number.  In fact, the continuation can include a
context that is outside of `foo', for example, we can invoke the
continuation from a different function, and `loop' can be used to search
for a specific number:

  (define (bar)
    (let ([x (foo)])
      (unless (> x 10) ((unbox loop)))
      x))

and now (bar) returns 11.  The loop is now essentially going over the
obvious part of `foo' but also over parts of `bar'.  Here's an example
that makes it even more obvious:

  (define (bar)
    (let* ([x (foo)]
           [y (* x 2)])
      (unless (> x 10) ((unbox loop)))
      y))

Since the `y' binding becomes part of the loop.  Our `foo' can be
considered as a kind of a producer for natural numbers that can be used
to find a specific number, invoking the `loop' continuation to try the
next number when the last one isn't the one we want.

========================================================================
>> The Ambiguous Operator: `amb'

Our `foo' is actually a very limited version of something that is known
as "McCarthy's Ambiguous Operator", usually named `amb'.  This operator
is used to perform a kind of a backtrack-able choice among several
values.

To develop our `foo' into such an `amb', we begin by renaming `foo' as
`amb' and `loop' as `fail', and instead of returning natural numbers in
sequence we'll have it take a list of values and return values from this
list.  Also, we will use mutable variables instead of boxes to reduce
clutter (a feature that we've mostly ignored so far).  The resulting
code is:

  (define fail #f)

  (define (amb choices)
    (let/cc k (set! fail k))
    (let ([choice (first choices)])
      (set! choices (rest choices))
      choice))

Of course, we also need to check that we actually have values to return:

  (define fail #f)

  (define (amb choices)
    (let/cc k (set! fail k))
    (if (pair? choices)
      (let ([choice (first choices)])
        (set! choices (rest choices))
        choice)
      (error "no more choices!")))

The resulting `amb' can be used in a similar way to the earlier `foo':

  (define (bar)
    (let* ([x (amb '(5 10 15 20))]
           [y (* x 2)])
      (unless (> x 10) (fail))
      y))
  (bar)

This is somewhat useful, but searching through a simple list of values
is not too exciting.  Specifically, we can have only one search at a
time.  Making it possible to have multiple searches is not too hard:
instead of a single failure continuation, store a stack of them, where
each new `amb' pushes a new one on it.

We define `failures' as this stack and push a new failure continuation
in each `amb'.  `fail' becomes a function that simply invokes the most
recent failure continuation, if one exists.

  (define failures null)

  (define (fail)
    (if (pair? failures)
      ((first failures))
      (error "no more choices!")))

  (define (amb choices)
    (let/cc k (set! failures (cons k failures)))
    (if (pair? choices)
      (let ([choice (first choices)])
        (set! choices (rest choices))
        choice)
      (error "no more choices!")))

This is close, but there's still something missing.  When we run out of
options from the `choices' list, we shouldn't just throw an error --
instead, we should invoke the previous failure continuation, if there is
one.  In other words, we want to use `fail', but before we do, we need
to pop up the top-most failure continuation since it is the one that we
are currently dealing with:

  (define failures null)

  (define (fail)
    (if (pair? failures)
      ((first failures))
      (error "no more choices!")))

  (define (amb choices)
    (let/cc k (set! failures (cons k failures)))
    (if (pair? choices)
      (let ([choice (first choices)])
        (set! choices (rest choices))
        choice)
      (begin (set! failures (rest failures))
             (fail))))

  (define (assert condition)
    (unless condition (fail)))

Note the addition of a tiny `assert' utility, something that is commonly
done with `amb'.  We can now play with this code as before:

  (let* ([x (amb '(5 10 15 20))]
         [y (* x 2)])
    (unless (> x 10) (fail))
    y)

But of course the new feature is more impressive, for example, find two
numbers that sum up to 6 and the first is the square of the second:

  (let ([a (amb '(1 2 3 4 5 6 7 8 9 10))]
        [b (amb '(1 2 3 4 5 6 7 8 9 10))])
    (assert (= 6 (+ a b)))
    (assert (= a (* b b)))
    (list a b))

Find a Pythagorean triplet:

  (let ([a (amb '(1 2 3 4 5 6))]
        [b (amb '(1 2 3 4 5 6))]
        [c (amb '(1 2 3 4 5 6))])
    (assert (= (* a a) (+ (* b b) (* c c))))
    (list a b c))

Specifying the list of integers is tedious, but easily abstracted into a
function:

  (let* ([int (lambda () (amb '(1 2 3 4 5 6)))]
         [a (int)]
         [b (int)]
         [c (int)])
    (assert (= (* a a) (+ (* b b) (* c c))))
    (list a b c))

A more impressive demonstration is finding a solution to tests known as
"Self-referential Aptitude Test", for example, here's one from
http://www.mozart-oz.org/home/doc/fdt/node38.html (by Christian Schulte
and Gert Smolka) -- it's a 10-question multiple choice test:

  1. The first question whose answer is b is question (a) 2; (b) 3; (c)
     4; (d) 5; (e) 6.
  2. The only two consecutive questions with identical answers are
     questions (a) 2 and 3; (b) 3 and 4; (c) 4 and 5; (d) 5 and 6; (e) 6
     and 7.
  3. The answer to this question is the same as the answer to question
     (a) 1; (b) 2; (c) 4; (d) 7; (e) 6.
  4. The number of questions with the answer a is (a) 0; (b) 1; (c) 2;
     (d) 3; (e) 4.
  5. The answer to this question is the same as the answer to question
     (a) 10; (b) 9; (c) 8; (d) 7; (e) 6.
  6. The number of questions with answer a equals the number of
     questions with answer (a) b; (b) c; (c) d; (d) e; (e) none of the
     above.
  7. Alphabetically, the answer to this question and the answer to the
     following question are (a) 4 apart; (b) 3 apart; (c) 2 apart; (d) 1
     apart; (e) the same.
  8. The number of questions whose answers are vowels is (a) 2; (b) 3;
     (c) 4; (d) 5; (e) 6.
  9. The number of questions whose answer is a consonant is (a) a prime;
     (b) a factorial; (c) a square; (d) a cube; (e) divisible by 5.
  10. The answer to this question is (a) a; (b) b; (c) c; (d) d; (e) e.

and the solution is pretty much a straightforward translation:

  (define (self-test)
    (define (choose-letter) (amb '(a b c d e)))
    (define q1  (choose-letter))
    (define q2  (choose-letter))
    (define q3  (choose-letter))
    (define q4  (choose-letter))
    (define q5  (choose-letter))
    (define q6  (choose-letter))
    (define q7  (choose-letter))
    (define q8  (choose-letter))
    (define q9  (choose-letter))
    (define q10 (choose-letter))
    ;; 1. The first question whose answer is b is question (a) 2; (b) 3;
    ;;    (c) 4; (d) 5; (e) 6.
    (assert (eq? q1 (cond [(eq? q2 'b) 'a]
                          [(eq? q3 'b) 'b]
                          [(eq? q4 'b) 'c]
                          [(eq? q5 'b) 'd]
                          [(eq? q6 'b) 'e]
                          [else (assert #f)])))
    ;; 2. The only two consecutive questions with identical answers
    ;;    are questions (a) 2 and 3; (b) 3 and 4; (c) 4 and 5; (d) 5
    ;;    and 6; (e) 6 and 7.
    (define all (list q1 q2 q3 q4 q5 q6 q7 q8 q9 q10))
    (define (count-same-consecutive l)
      (define (loop x l n)
        (if (null? l)
          n
          (loop (first l) (rest l)
                (if (eq? x (first l)) (add1 n) n))))
      (loop (first l) (rest l) 0))
    (assert (eq? q2 (cond [(eq? q2 q3) 'a]
                          [(eq? q3 q4) 'b]
                          [(eq? q4 q5) 'c]
                          [(eq? q5 q6) 'd]
                          [(eq? q6 q7) 'e]
                          [else (assert #f)])))
    (assert (= 1 (count-same-consecutive all))) ; exactly one
    ;; 3. The answer to this question is the same as the answer to
    ;;    question (a) 1; (b) 2; (c) 4; (d) 7; (e) 6.
    (assert (eq? q3 (cond [(eq? q3 q1) 'a]
                          [(eq? q3 q2) 'b]
                          [(eq? q3 q4) 'c]
                          [(eq? q3 q7) 'd]
                          [(eq? q3 q6) 'e]
                          [else (assert #f)])))
    ;; 4. The number of questions with the answer a is (a) 0; (b) 1;
    ;;    (c) 2; (d) 3; (e) 4.
    (define (count x l)
      (define (loop l n)
        (if (null? l)
          n
          (loop (rest l) (if (eq? x (first l)) (add1 n) n))))
      (loop l 0))
    (define num-of-a (count 'a all))
    (define num-of-b (count 'b all))
    (define num-of-c (count 'c all))
    (define num-of-d (count 'd all))
    (define num-of-e (count 'e all))
    (assert (eq? q4 (case num-of-a
                      [(0) 'a]
                      [(1) 'b]
                      [(2) 'c]
                      [(3) 'd]
                      [(4) 'e]
                      [else (assert #f)])))
    ;; 5. The answer to this question is the same as the answer to
    ;;    question (a) 10; (b) 9; (c) 8; (d) 7; (e) 6.
    (assert (eq? q5 (cond [(eq? q5 q10) 'a]
                          [(eq? q5 q9) 'b]
                          [(eq? q5 q8) 'c]
                          [(eq? q5 q7) 'd]
                          [(eq? q5 q6) 'e]
                          [else (assert #f)])))
    ;; 6. The number of questions with answer a equals the number of
    ;;    questions with answer (a) b; (b) c; (c) d; (d) e; (e) none
    ;;    of the above.
    (assert (eq? q6 (cond [(= num-of-a num-of-b) 'a]
                          [(= num-of-a num-of-c) 'b]
                          [(= num-of-a num-of-d) 'c]
                          [(= num-of-a num-of-e) 'd]
                          [else 'e])))
    ;; 7. Alphabetically, the answer to this question and the answer
    ;;    to the following question are (a) 4 apart; (b) 3 apart; (c)
    ;;    2 apart; (d) 1 apart; (e) the same.
    (define (choice->integer x)
      (case x [(a) 1] [(b) 2] [(c) 3] [(d) 4] [(e) 5]))
    (define (distance x y)
      (if (eq? x y)
        0
        (abs (- (choice->integer x) (choice->integer y)))))
    (assert (eq? q7 (case (distance q7 q8)
                      [(4) 'a]
                      [(3) 'b]
                      [(2) 'c]
                      [(1) 'd]
                      [(0) 'e]
                      [else (assert #f)])))
    ;; 8. The number of questions whose answers are vowels is (a) 2;
    ;;    (b) 3; (c) 4; (d) 5; (e) 6.
    (assert (eq? q8 (case (+ num-of-a num-of-e)
                      [(2) 'a]
                      [(3) 'b]
                      [(4) 'c]
                      [(5) 'd]
                      [(6) 'e]
                      [else (assert #f)])))
    ;; 9. The number of questions whose answer is a consonant is (a) a
    ;;    prime; (b) a factorial; (c) a square; (d) a cube; (e)
    ;;    divisible by 5.
    (assert (eq? q9 (case (+ num-of-b num-of-c num-of-d)
                      [(2 3 5 7) 'a]
                      [(1 2 6)   'b]
                      [(0 1 4 9) 'c]
                      [(0 1 8)   'd]
                      [(0 5 10)  'e]
                      [else (assert #f)])))
    ;; 10. The answer to this question is (a) a; (b) b; (c) c; (d) d;
    ;;     (e) e.
    (assert (eq? q10 q10)) ; (note: does nothing...)
    ;; The solution should be: (c d e b e e d c b a)
    all)

Note that the solution is simple because of the freedom we get with
continuations: the search is not a sophisticated one, but we're free to
introduce ambiguity points anywhere that fits, and mix assertions with
other code without worry about the control flow (which you would need to
in an implementation that uses explicit loops).  On the other hand, it
is not too efficient since it uses a naive search strategy.  (This could
be improved somewhat by deferring ambiguous points, for example, don't
assign q7, q8, q9, and q10 before the first question; but much of the
cost comes from the strategy for implementing continuation in Racket,
which makes capturing continuations a relatively expensive operation.)

When we started out with the modified loop, we had a representation of
an arbitrary natural number -- but with the move to lists of choices we
lost the ability to deal with such infinite choices.  Getting it back is
simple: delay the evaluation of the `amb' expressions.  We can do that
by switching to a list of thunks instead.  The change in the code is
just in the result: just return the result of calling `choice' instead
of returning it directly.  We can then rename `amb' to `amb/thunks' and
reimplement `amb' as a macro that wraps all of its sub-forms in thunks.

  (define (amb/thunks choices)
    (let/cc k (set! failures (cons k failures)))
    (if (pair? choices)
      (let ([choice (first choices)])
        (set! choices (rest choices))
        (choice))                    ; <-- call the choice thunk
      (begin (set! failures (rest failures))
             (fail))))

  (define-syntax-rule (amb E ...)
    (amb/thunks (list (lambda () E) ...)))

With this, we can implement code that computes choices rather than
having them listed:

  (define (integers-between n m)
    (assert (<= n m))
    (amb n (integers-between (add1 n) m)))

or even ones that are infinite:

  (define (integers-from n)
    (amb n (integers-from (add1 n))))

As with any infinite sequence, there are traps to avoid.  In this case,
trying to write code that can find any Pythagorean triplet as:

  (collect 7
    (let ([a (integers-from 1)]
          [b (integers-from 1)]
          [c (integers-from 1)])
      (assert (= (* a a) (+ (* b b) (* c c))))
      (list a b c)))

The problem is that the search loop will keep incrementing `c', and
therefore will not find any solution.  The search can work if only the
top-most choice is infinite:

  (collect 7
    (let* ([a (integers-from 1)]
           [b (integers-between 1 a)]
           [c (integers-between 1 a)])
      (assert (= (* a a) (+ (* b b) (* c c))))
      (list a b c)))

The complete code follows:

  ---<<<AMB>>>----------------------------------------------------------
  ;; The ambiguous operator and related utilities.

  #lang racket

  (define failures null)

  (define (fail)
    (if (pair? failures)
      ((first failures))
      (error "no more choices!")))

  (define (amb/thunks choices)
    (let/cc k (set! failures (cons k failures)))
    (if (pair? choices)
      (let ([choice (first choices)])
        (set! choices (rest choices))
        (choice))
      (begin (set! failures (rest failures))
             (fail))))

  (define-syntax-rule (amb E ...)
    (amb/thunks (list (lambda () E) ...)))

  (define (assert condition)
    (unless condition (fail)))

  (define (integers-between n m)
    (assert (<= n m))
    (amb n (integers-between (add1 n) m)))

  (define (integers-from n)
    (amb n (integers-from (add1 n))))

  (define (collect/thunk n thunk)
    (define results null)
    (let/cc too-few
      (set! failures (list too-few))
      (define result (thunk))
      (set! results (cons result results))
      (set! n (sub1 n))
      (unless (zero? n) (fail)))
    (set! failures null)
    (reverse results))

  (define-syntax collect
    (syntax-rules ()
      ;; collect N results
      [(_ N E) (collect/thunk N (lambda () E))]
      ;; collect all results
      [(_ E) (collect/thunk -1 (lambda () E))]))
  ----------------------------------------------------------------------

As a bonus, the code includes a `collect' tool that can be used to
collect a number of results -- it uses `fail' to iterate until a
sufficient number of values is collected:

  (define (collect/thunk n thunk)
    (define results null)
    (define result (thunk))
    (set! results (cons result results))
    (set! n (sub1 n))
    (unless (zero? n) (fail))
    (reverse results))

(Question: why does this code use mutation to collect the results?)

But since this might run into a premature failure, it installs its own
failure continuation that simply aborts the collection loop.  To try it
out:

  (collect (* (integers-between 1 3) (integers-between 1 5)))

========================================================================
>> Generators

Another popular facility that is related to continuations is generators.
The idea is to split code into separate "producers" and "consumers",
where the computation is interleaved between the two.  This makes for
simple solutions for some notoriously difficult problems.  It is also a
twist on the idea of co-routines, where two functions transfer control
back and forth as needed.  (Co-routines can be developed further into a
"cooperative threading" system, but we will not cover that here.)

A classical example that we have mentioned previously is the "same
fringe" problem.  One of the easy solutions that we talked about was to
run two processes that spit out the tree leaves, and a third process
that grabs both outputs as they come and compares them.  Using a lazy
language allowed a very similar solution, where the two processes are
essentially represented as two lazy lists.  But with continuations we
can find a solution that works in a strict language too, and in fact,
one that is very close to the two processes metaphor.

The fact that continuations can support such a solution shouldn't be
surprising: as with the kind of server-client interactions that we've
seen with the web language, and as with the `amb' tricks, the main theme
is the same -- the idea of suspending computation.  (Intuitively, this
also explain why a lazy language is related: it is essentially making
all computations suspendable in some sense.)

To implement generators, we begin with a simple code that we want to
eventually use:

  (define (producer)
    (yield 1)
    (yield 2)
    (yield 3))

where `yield' is expected to behave similarly to a `return' -- it should
make the function return 1 when called, and then somehow return 2 and 3
on subsequent calls.  To make it easier to develop, we'll make `yield'
an argument to the producer:

  (define (producer yield)
    (yield 1)
    (yield 2)
    (yield 3))

To use this producer, we need to find a proper value to call it with.
Sending it an identity, (lambda (x) x), is clearly not going to work: it
will make all `yield's executed on the first call, returning the last
value.  Instead, we need some way to abort the computation on the first
`yield'.  This, of course, can be done with a continuation, which we
should send as the value of the `yield' argument.  And indeed,

  > (let/cc k (producer k))
  1

returns 1 as we want.  The problem is that if we use this expression
again, we get more 1's as results:

  > (let/cc k (producer k))
  1
  > (let/cc k (producer k))
  1

The problem is obvious: our producer starts every time from scratch,
always sending the first value to the given continuation.  Instead, we
need to make it somehow save where it stopped -- its own continuation --
and on subsequent calls it should resume from that point.  We start to
do this by adding a `resume' continuation to save our position into:

  (define (producer yield)
    (define resume #f)
    (if (not resume)   ; we just started, so no resume yet
      (begin (yield 1)
             (yield 2)
             (yield 3))
      (resume 'blah))) ; we have a resume, use it

Next, we need to make it so that each use of `yield' will save its
continuation as the place to resume from:

  (define (producer yield)
    (define resume #f)
    (if (not resume)
      (begin (let/cc k (set! resume k) (yield 1))
             (let/cc k (set! resume k) (yield 2))
             (let/cc k (set! resume k) (yield 3)))
      (resume 'blah)))

But this is still broken in an obvious way: every time we invoke this
function, we define a new local `resume' which is always `#f', leaving
us with the same behavior.  We need `resume' to persist across calls --
which we can get by "pulling it out" using a `let':

  (define producer
    (let ([resume #f])
      (lambda (yield)
        (if (not resume)
          (begin (let/cc k (set! resume k) (yield 1))
                 (let/cc k (set! resume k) (yield 2))
                 (let/cc k (set! resume k) (yield 3)))
          (resume 'blah)))))

And this actually works:

  > (let/cc k (producer k))
  1
  > (let/cc k (producer k))
  2
  > (let/cc k (producer k))
  3

(Tracing how it works is a good exercise.)

Before we continue, we'll clean things up a little.  First, to make it
easier to get values from the producer, we can write a little helper:

  (define (get producer)
    (let/cc k (producer k)))

Next, we can define a local helper inside the producer to improve it in
a similar way by making up a `yield' that wraps the `raw-yield' input
continuation (also flip the condition):

  (define producer
    (let ([resume #f])
      (lambda (raw-yield)
        (define (yield value)
          (let/cc k (set! resume k) (raw-yield value)))
        (if resume
          (resume 'blah)
          (begin (yield 1)
                 (yield 2)
                 (yield 3))))))

And we can further abstract out the general producer code from the
specific 1-2-3 producer that we started with.  The complete code is now:

  (define (make-producer producer)
    (let ([resume #f])
      (lambda (raw-yield)
        (define (yield value)
          (let/cc k (set! resume k) (raw-yield value)))
        (if resume
          (resume 'blah)
          (producer yield)))))

  (define (get producer)
    (let/cc k (producer k)))

  (define producer
    (make-producer (lambda (yield)
                     (yield 1)
                     (yield 2)
                     (yield 3))))

When we now evaluate (get producer) three times, we get back the three
values in the correct order.  But there is a subtle bug here, first try
this (after re-running!):

  > (list (get producer) (get producer))

Seems that this is stuck in an infinite loop.  To see where the problem
is, re-run to reset the producer, and then we can see the following
interaction:

  > (* 10 (get producer))
  10
  > (* 100 (get producer))
  20
  > (* 12345 (get producer))
  30

This looks weird...  Here's a more clarifying example:

  > (list (get producer))
  '(1)
  > (get producer)
  '(2)
  > (get producer)
  '(3)

Can you see what's wrong now?  It seems that all three invocations of
the producer use the same continuation -- the first one, specifically,
the (list <*>) continuation.  This also explains why we run into an
infinite loop with (list (get producer) (get producer)) -- the first
continuation is:

  (list <*> (get producer))

so when we get the first `1' result we plug it in and proceed to
evaluate the second (get producer), but that re-invokes the *first*
continuation again, getting into an infinite loop.  We need to look
closely at our `make-producer' to see the problem:

  (define (make-producer producer)
    (let ([resume #f])
      (lambda (raw-yield)
        (define (yield value)
          (let/cc k (set! resume k) (raw-yield value)))
        (if resume
          (resume 'blah)
          (producer yield)))))

When (make-producer (lambda (yield) ...)) is first called, `resume' is
initialized to `#f', and the result is the (lambda (raw-yield) ...),
which is bound to the global `producer'.  Next, we call this function,
and since `resume' is `#f', we apply the `producer' on our `yield' --
which is a closure that has a reference to the `raw-yield' that we
received -- the continuation that was used in this first call.  The
problem is that on subsequent calls `resume' will contain a continuation
which it is called, but this will jump back to that first closure with
the original `raw-yield', so instead of returning to the current calling
context, we re-return to the first context -- the same first
continuation.  The code can be structured slightly to make this a little
more obvious: push the `yield' definition into the only place it is used
(the first call):

  (define (make-producer producer)
    (let ([resume #f])
      (lambda (raw-yield)
        (if resume
          (resume 'blah)
          (let ([yield (lambda (value)
                         (let/cc k (set! resume k) (raw-yield value)))])
            (producer yield))))))

`yield' is not used elsewhere, so this code has exactly the same meaning
as the previous version.  You can see now that when the producer is
first used, it gets a `raw-yield' continuation which is kept in a newly
made closure -- and even though the following calls have *different*
continuations, we keep invoking the first one.  These calls get new
continuations as their `raw-yield' input, but they ignore them.  It just
happened that the when we evaluated (get producer) three times on the
REPL, all calls had essentially the same continuation (the `P' part of
the REPL), so it seemed like things are working fine.

To fix this, we must avoid calling the same initial `raw-yield' every
time: we must change it with each call so it is the right one.  We can
do this with another mutation -- introduce another state variable that
will refer to the correct `raw-yield', and update it on every call to
the producer.  Here's one way to do this:

  (define (make-producer producer)
    (let ([resume #f]
          [return-to-caller #f])
      (lambda (raw-yield)
        (set! return-to-caller raw-yield)
        (if resume
          (resume 'blah)
          (let ([yield (lambda (value)
                         (let/cc k
                           (set! resume k)
                           (return-to-caller value)))])
            (producer yield))))))

Using this, we get well-behaved results:

  > (list (get producer))
  '(1)
  > (* 8 (get producer))
  16
  > (get producer)
  3

or (again, after restarting the producer by re-running the code):

  > (list (get producer) (get producer) (get producer))
  '(1 2 3)

Side-note: a different way to achieve this is to realize that when we
invoke `resume', we're calling the continuation that was captured by the
`let/cc' expression.  Currently, we're sending just 'blah to that
continuation, but we could send `raw-yield' there instead.  With that,
we can make that continuation be the target of setting the
`return-to-caller' state variable.  (This is how PLAI solves this
problem.)

  (define (make-producer producer)
    (let ([resume #f])
      (lambda (raw-yield)
        (define return-to-caller raw-yield)
        (define (yield value)
          (set! return-to-caller
                (let/cc k
                  (set! resume k)
                  (return-to-caller value))))
        (if resume
          (resume raw-yield)
          (producer yield)))))

Continuing with our previous code, and getting the `yield' back into a a
more convenient definition form, we have this complete code:

  ---<<<PRODUCER>>>-----------------------------------------------------
  ;; An implementation producer functions

  #lang racket

  (define (make-producer producer)
    (let ([resume #f]
          [return-to-caller #f])
      (lambda (raw-yield)
        (define (yield value)
          (let/cc k (set! resume k) (return-to-caller value)))
        (set! return-to-caller raw-yield)
        (if resume
          (resume 'blah)
          (producer yield)))))

  (define (get producer)
    (let/cc k (producer k)))

  (define producer
    (make-producer (lambda (yield)
                     (yield 1)
                     (yield 2)
                     (yield 3))))
  ----------------------------------------------------------------------

There is still a small problem with this code:

  > (list (get producer) (get producer) (get producer))
  '(1 2 3)
  > (get producer)
  ;; infinite loop

Tracking this problem is another good exercise, and finding a solution
is easy.  (For example, throwing an error when the producer is
exhausted, or returning 'done, or returning the return value of the
producer function.)

========================================================================
>> Delimited Continuations

While the continuations that we have seen are a useful tool, they are
often "too global" -- they capture the complete computation context.
But in many cases we don't want that, instead, we want to capture a
specific context.  In fact, this is exactly why producer code got
complicated: we needed to keep capturing the `return-to-caller'
continuation to make it possible to return to the correct context rather
than re-invoking the initial (and wrong) context.

Additional work on continuations resulted in a feature that is known as
"delimited continuations".  These kind of continuations are more
convenient in that they don't capture the complete context -- just a
potion of it up to a specific point.  To see how this works, we'll
restart with a relatively simple producer definition:

  (define producer
    (let ()
      (define (cont)
        (let/cc ret
          (define (yield value)
            (let/cc k (set! cont k) (ret value)))
          (yield 1)
          (yield 2)
          (yield 3)
          4))
      (define (generator) (cont))
      generator))

This producer is essentially the same as one that we've seen before: it
seems to work in that it returns the desired values for every call:

  > (producer)
  1
  > (producer)
  2
  > (producer)
  3

But fails in that it always returns to the initial context:

  > (list (producer))
  '(1)
  > (+ 100 (producer))
  '(2)
  > (* "bogus" (producer))
  '(3)

Fixing this will lead us down the same path we've just been through: the
problem is that `generator' is essentially an indirection "trampoline"
function that goes to whatever `cont' currently holds, and except for
the initial value of `cont' the other values are continuations that are
captured inside `yield', meaning that the calls are all using the same
`ret' continuation that was grabbed once, at the beginning.  To fix it,
we will need to re-capture a return continuation on every use of
`yield', which we can do by modifying the `ret' binding, giving us a
working version:

  (define producer
    (let ()
      (define (cont)
        (let/cc ret
          (define (yield value)
            (let/cc k
              (set! cont (lambda () (let/cc r (set! ret r) (k))))
              (ret value)))
          (yield 1)
          (yield 2)
          (yield 3)
          4))
      (define (generator) (cont))
      generator))

This pattern of grabbing the current continuation and then jumping to
another -- (let/cc k (set! cont k) (ret value)) -- is pretty common,
enough that there is a specific construct that does something similar:
`control'.  Translating the `let/cc' form to it produces:

  (control k (set! cont ...) value)

A notable difference here is that we don't use a `ret' continuation.
Instead, another feature of the `control' form is that the value returns
to a specific point back in the current computation context that is
marked with a `prompt'.  (Note that the `control' and `prompt' bindings
are not included in the default `racket' language, we need to get them
from a library: (require racket/control).)  The fully translated code
simply uses this `prompt' in place of the outer capture of the `ret'
continuation:

  (define producer
    (let ()
      (define (cont)
        (prompt
          (define (yield value)
            (control k
              (set! cont ???)
              value))
          (yield 1)
          (yield 2)
          (yield 3)
          4))
      (define (generator) (cont))
      generator))

We also need to translate the (lambda () (let/cc r (set! ret r) (k)))
expression -- but there is no `ret' to modify.  Instead, we get the same
effect by another use of `prompt' which is essentially modifying the
implicitly used return continuation:

  (define producer
    (let ()
      (define (cont)
        (prompt
          (define (yield value)
            (control k
              (set! cont (lambda () (prompt (k))))
              value))
          (yield 1)
          (yield 2)
          (yield 3)
          4))
      (define (generator) (cont))
      generator))

This looks like the previous version, but there's an obvious advantage:
since there is no `ret' binding that we need to maintain, we can pull
out the `yield' definition to a more convenient place:

  (define producer
    (let ()
      (define (yield value)
        (control k
          (set! cont (lambda () (prompt (k))))
          value))
      (define (cont)
        (prompt
          (yield 1)
          (yield 2)
          (yield 3)
          4))
      (define (generator) (cont))
      generator))

Note that this is an important change, since the producer machinery can
now be abstracted into a `make-producer' function, as we've done before:

  (define (make-producer producer)
    (define (yield value)
      (control k
        (set! cont (lambda () (prompt (k))))
        value))
    (define (cont) (prompt (producer yield)))
    (define (generator) (cont))
    generator)

  (define producer
    (make-producer (lambda (yield)
                     (yield 1)
                     (yield 2)
                     (yield 3)
                     4)))

This is, again, a common pattern in such looping constructs -- where the
continuation of the loop keeps modifying the prompt as we do in the
thunk assigned to `cont'.  There are two other operators that are
similar to `control' and `prompt', which re-instate the point to return
to automatically.  Confusingly, they have completely different name:
`shift' and `reset'.  In the case of our code, we simply do the
straightforward translation, and drop the extra wrapping step inside the
value assigned to `cont' since that is done automatically.  The
resulting definition becomes even shorter now:

  (define (make-producer producer)
    (define (yield value) (shift k (set! cont k) value))
    (define (cont) (reset (producer yield)))
    (define (generator) (cont))
    generator)

(Question: which set of forms is the more powerful one?)

It even looks like this code works reasonably well when the producer is
exhausted:

  > (list (producer) (producer) (producer) (producer) (producer))
  '(1 2 3 4 4)

But the problem is still there, except a but more subtle.  We can see it
if we add a side-effect:

  (define producer
    (make-producer (lambda (yield)
                     (yield 1)
                     (yield 2)
                     (yield 3)
                     (printf "Hey!\n")
                     4)))

and now we get:

  > (list (producer) (producer) (producer) (producer) (producer))
  Hey!
  Hey!
  '(1 2 3 4 4)

This can be solved in the same way as we've discussed earlier -- for
example, grab the result value of the producer (which means that we get
the value only after it's exhausted), then repeat returning that value.
A particularly easy way to do this is to set `cont' to a thunk that
returns the value -- since the resulting `generator' function simply
invokes it, we get the desired behavior of returning the last value on
further calls:

  (define (make-producer producer)
    (define (yield value) (shift k (set! cont k) value))
    (define (cont)
      (reset (let ([retval (producer yield)])
               ;; we get here when the producer is done
               (set! cont (lambda () retval))
               retval)))
    (define (generator) (cont))
    generator)

  (define producer
    (make-producer (lambda (yield)
                     (yield 1)
                     (yield 2)
                     (yield 3)
                     (printf "Hey!\n")
                     4)))

and now we get the improved behavior:

  > (list (producer) (producer) (producer) (producer) (producer))
  Hey!
  '(1 2 3 4 4)

========================================================================
>>> Continuation Conclusions

Continuations are often viewed as a feature that is too complicated to
understand and/or are hard to implement.  As a result, very few
languages provide general first-class continuations.  Yet, they are an
extremely useful tool since they enable implementing new kinds of
control operators as user-written libraries.  The "user" part is
important here: if you want to implement producers (or a convenient
`web-read', or an ambiguous operator, or any number of other uses) in a
language that doesn't have continuations your options are very limited.
You can ask for the new feature and wait for the language implementors
to provide it, or you can CPS the relevant code (and the latter option
is possible only if you have complete control over the whole code source
to transform).  With continuations, as we have seen, it is not only
possible to build such libraries, the resulting functionality is as if
the language has the desired feature already built-in.  For example,
Racket comes with a generator library that is very similar to Python
generators -- but in contrast to Python, it is implemented completely in
user code.  (In fact, the implementation is very close to the delimited
continuations version that we've seen last.)

Obviously, in cases where you don't have continuations and you need them
(or rather when you need some functionality that is implementable via
continuations), you will likely resort to the CPS approach, in some
limited version.  For example, the Racket documentation search page (eg,
http://docs.racket-lang.org/search/) allows input to be typed while the
search is happening.  This is a feature that by itself is not available
in JavaScript -- it is as if there are two threads running (one for the
search and one to handle input), where JS is single-threaded on
principle.  This was implemented by making the search code side-effect
free, then CPS-ing the code, then mimic threads by running the search
for a bit, then storing its (manually built) continuation, handling
possible new input, then resuming the search via this continuation.  An
approach that solves a similar problem using a very different approach
is node.js -- a JavaScript-based server where all IO is achieved via
functions that receive callback functions, resulting in a style of code
that is essentially writing CPSed code.  For example, it is similar in
principle to write code like:

  ;; copy "foo" to "tmp", read a line, delete "tmp", log the line
  (copy-file "foo" "tmp"
    (lambda ()
      (read-line "tmp"
        (lambda (line)
          (delete-file "tmp"
            (lambda ()
              (log-line line
                (lambda ()
                  (printf "All done.\n")))))))))

or a concrete node.js example:

  function swap(path1, path2, callback) {
    fs.rename(path1, "temp-name",
      function() {
        fs.rename(path2, path1,
          function() {
            fs.rename("temp-name", path2, callback);
          });
      });
  }

As we have seen in the web server application example, this style of
programming tends to be "infectious", where a function that deals with
these callback-based functions will itself consume a callback --

  ;; abstract the above as a function
  (define (safe-log-line in-file callback)
    (copy-file in-file "tmp"
      (lambda ()
        ... (log-line line callback))))

(You should be able to see now what is happening here, without even
mentioning the word "continuation" in the docs...)

Finally, as mentioned a few times, there has been extensive research
into many aspects of continuations.  Different CPS approaches, different
implementation strategies, a whole zoo of control operators (see the
second part of http://docs.racket-lang.org/reference/cont.html),
assigning types to continuation-related functions and connections
between continuations and types, even connections between CPS and
certain proof techniques.  Some research is still going on, though not
as hot as it was -- but more importantly, many modern languages
"discover" the utility of having continuations, sometimes in some more
limited forms (eg, Python generators), and sometimes in full form (eg,
Ruby's `callcc').

========================================================================
