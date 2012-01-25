---
title: Thoughts on text and text editors
filemdate: 2012.01.16
---

*this page contains some ideas about text processing and the creation of text editors*

{menu: }

Interesting text editors
========================

Emacs
-----
TBD


Vi 
----
TBD

EDT
----

TBD


Textpad
-------
TBD


To Modes or Not to Modes
========================
TBD


My own stupid offering
======================
TBD

Joe on Erlang and Text
======================

> Well, if I were doing this, I probably wouldn't do it in 
> Erlang.  Text handling in Erlang is byzantine at best, so 
> an editor very well could be an exercise in pain. 

I beg to differ. 

In my opinion Erlang is brilliant at handling text - text is stored in lists and list processing is blindingly fast and there are large numbers of library functions that work on lists. A very unpainful experience. 

I have written a full text edit in both Erlang and C - the C *was* painful Imagine how to implement "undo" in Erlang.  Since data is immutable, you just revert to some old state by accessing the  data at some historical point in the edit - trail all old states in a history stack and a difficult operation become trivial to program (and efficient because of the sharing) by popping the history stack. Easy in Erlang - difficult in a language with mutable state.

I have written *many* text processing applications in Erlang and only once had problems. My usual experience is that I can write mind-boggelingly inefficient code which never the less executes blindingly fast. I always use the get-it-right then optimize philosophy. But the number of times I have to optimize is very very few. For text applications virtually *never*. Text processing is trivial (unless you're talking of Gigabytes, but then the problem is one of algorithms) 

Only one text application ever caused me a problem - writing a full text indexer for Gigabytes of data - the problem was not with Erlang but with my knowledge of algorithms - after I read "managing gigabyes" and implemented gamma encoding in Erlang the problem went away. (the ease of which I could change representations for lists, to (in this case) gamma encoded integers, was a big big win) Text process applications involve mainly list processing. 

If lists don't cut it then custom abstractions can be easily made. Erlang is *brilliant* at text applications. Lists are blindingly fast. The bit syntax is fantastic if you need to do things like huffman encoding or the like for tricky text representations. Complex data structures are trivial to create. A text editor hardy needs concurrency it should be way fast enough on one core - there is no "natural concurrency in the problem" - emacs used to run blindingly fast on a 40Mz PC with 128KB of memory - it's *not* a difficult problem making something like emacs run fast enough (unless you want to use emacs to search through GBytes of data, in which case you're probably should not be using emacs anyway) 

I wrote a simple emacs editor years ago - it's in the widgets subdirectory of http://www.sics.se/~joe/ex11/download/release-2.5.tgz The only tricky part was not the emacs logic, but the screen display and catching the keystrokes and mouse events. 

/Joe 

References
==========

- Finseth
- Emacs pdf

