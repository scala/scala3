dotty
=====

[![Join the chat at https://gitter.im/lampepfl/dotty](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/lampepfl/dotty?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Dotty is a platform to try out new language concepts and compiler
technologies for Scala. The focus is mainly on simplification. We
remove extraneous syntax (e.g. no XML literals), and try to boil down
Scala's types into a smaller set of more fundamental constructors. The
theory behind these constructors is researched in
[DOT](http://www.cs.uwm.edu/~boyland/fool2012/papers/fool2012_submission_3.pdf),
a calculus for dependent object types.

####Current status:
 _Technology preview_: currently unsupported, may not be functionally complete, and are not suitable for deployment in production.

####Is it going to be the future Scala?
Yes, eventually.

####Who's working on it?
See [github contributors page](https://github.com/lampepfl/dotty/graphs/contributors).
 
####What are the features that could make me consider trying it?  
| Feature                                         	| Status              	|
|-------------------------------------------------	|---------------------	|
| Union, Intersection and Literal singleton types 	| Implemented         	|
| Fast compilation (phase fusion)                 	| Implemented         	|
| Working contravariant implicits                 	| Implemented         	|
| Trait parameters                                	| Implemented         	|
| @Static methods and fields                      	| Implemented         	|
| Colored Repl                                    	| Implemented         	|
| Sbt incremental build                           	| Implemented         	|
| Non-blocking lazy vals                          	| Implemented         	|
|                                                 	|                     	|
| Non-boxed arrays of value classes               	| In progress         	|
| Auto-Specialization                             	| In progress         	|
| Whole program optimizer                         	| In progress         	|
|                                                 	|                     	|
| HList & HMaps\Record types                      	| Under consideration 	|
| Implicit functions                              	| Under consideration 	|
| Effects                                         	| Under consideration 	|
| Auto-completion in repl                         	| Under consideration 	|
| Spec name-based patmat                          	| Under consideration 	|
| Multiverse equality                             	| Under consideration 	|
| Exhaustivity checks in pattern matching          	| Under consideration 	|
There are also plethora of small details such [per-callsite @tailrec annotations]

####What are the complications that I can have If I start using Dotty?
Dotty can use libraries compiled by scalac 2.11, but Scala scalac can't use libraries compiled by Dotty.<br>
No existential types.<br>
No macro support yet. We have big plans here.<br>
No early initializers. No scala.DelayedInit. Use trait arguments instead.<br>
Whole program optimizer will only work if all dependencies are compiled by Dotty.<br>


####Can I write my code in a way that is going to be compatible with Scalac & Dotty?
Yes, Dotty itself is a project that can be compiled by both Dotty and Scalac.<br>
It's not very hard, and the biggest thing that you will likely miss is using macros.

####How can I try it out?
https://github.com/lampepfl/dotty/wiki/Getting-Started.
Here’s example sbt project and instructions on how to set it up: https://github.com/smarter/dotty-example-project/ <br>
We have colored REPL :-). You can invoke it by running `dotc -repl`.

####We also have:
Basic support for Scala.js,<br>
[Prototype](https://github.com/scala-native/scala-native/tree/topic/dotty-support) of compilation to x86 native code(Shabalin)<br>

####What about scalac:
Scalac is basis for stability in scala. We expect scalac & dotty to coexist for long.
        
####Contributions are welcome!
We invite you to help us build the future of Scala.<br>
That's the best moment to participate, as everyone can make an impact.<br>

####SI-2712?
If scalac will put it into 2.12, we’ll mimic their behaviour. But we have bigger plans for
HK-types.



Developers mailing list is https://groups.google.com/forum/#!forum/dotty-internals.
