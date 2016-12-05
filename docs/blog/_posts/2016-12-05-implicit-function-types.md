---
layout: blog
title: Implicit Function Types
author: Martin Odersky
authorImg: /images/martin.jpg
---

I just made the first pull request to add _implicit function types_ to
Scala. I am pretty excited about it, because, citing the explanation
of the pull request "This is the first step to bring comonadic
abstraction to Scala". That's quite a mouthful, so I better explain what I
mean by it.

Let me try to explain the words in this sentence from right to left.

*Scala*: I assume everyone who reads this understands that we mean the
 programming language, not the opera house.

*Abstraction*: The ability to name a concept and use just the name afterwards.

*Comonadic*: In category theory, a _comonad_ is the dual of a
_monad_. Roughly speaking, a monad is a way to wrap the result (or:
outputs) of a computation in some other type. For instance
`Future[T]` means that the result of type `T` will be produced at
some later time on demand, or `Option[T]` indicates that the result
might also be undefined.

Dually, a _comonad_ allows to transform or
enrich or otherwise manipulate the _inputs_ to a computation.
The inputs are typically what a computation can access in its
environment. Interesting tasks that are by nature comonadic are

 - passing configuration data to the parts of a system that need them,
 - managing capabilities for security critical tasks,
 - wiring components up with dependency injection,
 - defining the meanings of operations with type classes,
 - more generally, passing any sort of context to a computation.

Implicit function types are a suprisingly simple and general way to
make coding patterns solving these tasks abstractable, reducing
boilerplate code and increasing applicability.

*First Step* My pull request is first implementation. In solves the
 problem in principle, but it introduces some run-time overhead. The
 next step will be to eliminate the run-time overhead through some
 simple optimizations.


## Comparison with Monads

One can use monads for these tasks, and some people do. For instance
the `Reader` monad is used to abstract over accessing one entry in the
environment.  But the code for doing so quickly becomes complex and
inefficient, in particular when combining several contextual
accesses. Monads don't compose in general, and therefore even simple
combinations need to be expressed on the level of monad transformers,
at the price of much boilerplate and complexity. Recognizing this,
peaple have recently experimented with free monads, which alleviate
the composibility problem, but at the price of introducing a whole new
level of interpretation.

## Implicit Parameters

In a functional setting, the inputs to a computation are most
naturally expressed as _parameters_. One could simply augment
functions to take additional parameters that represent configurations,
capabilities, dictionaries, or whatever contextual data the functions
need. The only downside with this is that often there's a large
distance in the call graph between the definition of a contextual
element and the site where it is used. Conseuqently, it becomes
tedious to define all those intermediate parameters and to pass them
along to where they are eventually consumed.

Implicit parameters solve one half of the problem. Implicit
parameters do not have to be propagated using boilerplate code; the
compiler takes care of that. This makes them practical in many
scenarios where plain parameters would be too cumbersome. For
instance, type classes would be a lot less popular if one would have
to pass all dictionaries by hand. Implicit parameters are also very
useful as a general context passing mechanism. For instance in the
_dotty_ compiler, almost every function takes an implicit context
parameter which defines all elements relating to the current state of
the compilation. This is in my experience much better than the cake
pattern because it is lightweight and can express context changes in a
purely functional way.

The main downside of implicit parameters is the verbosity of their
declaration syntax. It's hard to illustrate this with a smallish example,
because it really only becomes a problem at scale, but let's try anyway.

Let's say we want to write some piece of code that's designed to run
in a transaction. For the sake of illustration here's a simple transaction class:

    class Transaction {
      private val log = new ListBuffer[String]
      def println(s: String): Unit = log += s

      private var aborted = false
      private var committed = false

      def abort(): Unit = { aborted = true }
      def isAborted = aborted

      def commit(): Unit =
        if (!aborted && !committed) {
          Console.println("******* log ********")
          log.foreach(Console.println)
          committed = true
        }
    }

The transaction encapsulates a log, to which one can print messages.
It can be in one of three states: running, committed, or aborted.
If the transaction is committed, it prints the stored log to the console.

The `transaction` method lets one run some given code `op` inside
a newly created transaction:

      def transaction[T](op: Transaction => T) = {
        val trans: Transaction = new Transaction
        op(trans)
        trans.commit()
      }

The current transaction needs to be passed along a calling chain to all
the places that need to access it. To illustrate this, here are three
functions `f1`, `f2` and `f3` which call each other, and also access
the current transaction. The most convenient way to achieve this is
passing the current transaction as an implicit parameter.

      def f1(x: Int)(implicit thisTransaction: Transaction): Int = {
        thisTransaction.println(s"first step: $x")
        f2(x + 1)
      }
      def f2(x: Int)(implicit thisTransaction: Transaction): Int = {
        thisTransaction.println(s"second step: $x")
        f3(x * x)
      }
      def f3(x: Int)(implicit thisTransaction: Transaction): Int = {
        thisTransaction.println(s"third step: $x")
        if (x % 2 != 0) thisTransaction.abort()
        x
      }

The main program calls `f1` in a fresh transaction context and prints
its result:

      def main(args: Array[String]) = {
        transaction {
          implicit thisTransaction =>
            val res = f1(args.length)
            println(if (thisTransaction.isAborted) "aborted" else s"result: $res")
        }
      }

Two sample calls of the program (let's call it `TransactionDemo`) are here:

    scala TransactionDemo 1 2 3
    result: 16
    ******* log ********
    first step: 3
    second step: 4
    third step: 16

    scala TransactionDemo 1 2 3 4
    aborted




