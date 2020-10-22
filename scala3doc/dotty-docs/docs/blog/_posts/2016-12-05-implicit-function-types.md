---
layout: blog-page
title: Implicit Function Types
author: Martin Odersky
authorImg: images/martin.jpg
date: 2016-12-05
---

I just made the [first pull request](https://github.com/lampepfl/dotty/pull/1775) to add _implicit function types_ to
Scala. I am pretty excited about it, because - citing the explanation
of the pull request - "_This is the first step to bring contextual
abstraction to Scala_". What do I mean by this?

**Abstraction**: The ability to name a concept and use just the name afterwards.

**Contextual**: A piece of a program produces results or outputs in
some context. Our programming languages are very good at describing
and abstracting what outputs are produced. But there's hardly anything
yet available to abstract over the inputs that programs get from their
context. Many interesting scenarios fall into that category,
including:

 - passing configuration data to the parts of a system that need them,
 - managing capabilities for security critical tasks,
 - wiring components up with dependency injection,
 - defining the meanings of operations with type classes,
 - more generally, passing any sort of context to a computation.

Implicit function types are a surprisingly simple and general way to
make coding patterns solving these tasks abstractable, reducing
boilerplate code and increasing applicability.

**First Step**: My pull request is a first implementation. It solves the
 problem in principle, but introduces some run-time overhead. The
 next step will be to eliminate the run-time overhead through some
 simple optimizations.

## Implicit Parameters

In a functional setting, the inputs to a computation are most
naturally expressed as _parameters_. One could simply augment
functions to take additional parameters that represent configurations,
capabilities, dictionaries, or whatever contextual data the functions
need. The only downside with this is that often there's a large
distance in the call graph between the definition of a contextual
element and the site where it is used. Consequently, it becomes
tedious to define all those intermediate parameters and to pass them
along to where they are eventually consumed.

Implicit parameters solve one half of the problem. Implicit
parameters do not have to be propagated using boilerplate code; the
compiler takes care of that. This makes them practical in many
scenarios where plain parameters would be too cumbersome. For
instance, type classes would be a lot less popular if one would have
to pass all dictionaries by hand. Implicit parameters are also very
useful as a general context passing mechanism. For instance in the
_Dotty_ compiler, almost every function takes an implicit context
parameter which defines all elements relating to the current state of
the compilation. This is in my experience much better than the cake
pattern because it is lightweight and can express context changes in a
purely functional way.

The main downside of implicit parameters is the verbosity of their
declaration syntax. It's hard to illustrate this with a smallish example,
because it really only becomes a problem at scale, but let's try anyway.

Let's say we want to write some piece of code that's designed to run
in a transaction. For the sake of illustration here's a simple transaction class:
```scala
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
```
The transaction encapsulates a log, to which one can print messages.
It can be in one of three states: running, committed, or aborted.
If the transaction is committed, it prints the stored log to the console.

The `transaction` method lets one run some given code `op` inside
a newly created transaction:
```scala
  def transaction[T](op: Transaction => T) = {
    val trans: Transaction = new Transaction
    op(trans)
    trans.commit()
  }
```
The current transaction needs to be passed along a call chain to all
the places that need to access it. To illustrate this, here are three
functions `f1`, `f2` and `f3` which call each other, and also access
the current transaction. The most convenient way to achieve this is
by passing the current transaction as an implicit parameter.
```scala
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
```
The main program calls `f1` in a fresh transaction context and prints
its result:
```scala
  def main(args: Array[String]) = {
    transaction {
      implicit thisTransaction =>
        val res = f1(args.length)
        println(if (thisTransaction.isAborted) "aborted" else s"result: $res")
    }
  }
```
Two sample calls of the program (let's call it `TransactionDemo`) are here:
```shell
scala TransactionDemo 1 2 3
result: 16
******* log ********
first step: 3
second step: 4
third step: 16

scala TransactionDemo 1 2 3 4
aborted
```
So far, so good. The code above is quite compact as far as expressions
are concerned. In particular, it's nice that, being implicit
parameters, none of the transaction values had to be passed along
explicitly in a call. But on the definition side, things are less
rosy: Every one of the functions `f1` to `f3` needed an additional
implicit parameter:
```scala
(implicit thisTransaction: Transaction)
```
A three-times repetition might not look so bad here, but it certainly
smells of boilerplate. In real-sized projects, this can get much worse.
For instance, the _Dotty_ compiler uses implicit abstraction
over contexts for most of its parts. Consequently it ends up with currently
no fewer than 2641 occurrences of the text string
```scala
(implicit ctx: Context)
```
It would be nice if we could get rid of them.

## Implicit Functions

Let's massage the definition of `f1` a bit by moving the last parameter section to the right of the equals sign:
```scala
  def f1(x: Int) = { implicit thisTransaction: Transaction =>
    thisTransaction.println(s"first step: $x")
    f2(x + 1)
  }
```
The right hand side of this new version of `f1` is now an implicit
function value. What's the type of this value? Previously, it was
`Transaction => Int`, that is, the knowledge that the function has an
implicit parameter got lost in the type. The main extension implemented by
the pull request is to introduce implicit function types that mirror
the implicit function values which we have already. Concretely, the new
type of `f1` is:
```scala
implicit Transaction => Int
```
Just like the normal function type syntax `A => B`, desugars to `scala.Function1[A, B]`
the implicit function type syntax `implicit A => B` desugars to `scala.ImplicitFunction1[A, B]`.
The same holds at other function arities. With Dotty's [pull request #1758](https://github.com/lampepfl/dotty/pull/1758)
merged there is no longer an upper limit of 22 for such functions.

The type `ImplicitFunction1` can be thought of being defined as follows:
```scala
trait ImplicitFunction1[-T0, R] extends Function1[T0, R] {
  override def apply(implicit x: T0): R
}
```
However, you won't find a classfile for this trait because all implicit function traits
get mapped to normal functions during type erasure.

There are two rules that guide type checking of implicit function types.
The first rule says that an implicit function is applied to implicit arguments
in the same way an implicit method is. More precisely, if `t` is an expression
of an implicit function type
```scala
t: implicit (T1, ..., Tn) => R
```
such that `t` is not an implicit closure itself and `t` is not the
prefix of a call `t.apply(...)`, then an `apply` is implicitly
inserted, so `t` becomes `t.apply`. We have already seen that the
definition of `t.apply` is an implicit method as given in the
corresponding implicit function trait. Hence, it will in turn be
applied to a matching sequence of implicit arguments. The end effect is
that references to implicit functions get applied to implicit arguments in the
same way as references to implicit methods.

The second rule is the dual of the first. If the expected type
of an expression `t` is an implicit function type
```scala
implicit (T1, ..., Tn) => R
```
then `t` is converted to an implicit closure, unless it is already one.
More precisely, `t` is mapped to the implicit closure
```scala
implicit ($ev1: T1, ..., $evn: Tn) => t
```
The parameter names of this closure are compiler-generated identifiers
which should not be accessed from user code. That is, the only way to
refer to an implicit parameter of a compiler-generated function is via
`implicitly`.

It is important to note that this second conversion needs to be applied
_before_ the expression `t` is typechecked. This is because the
conversion establishes the necessary context to make type checking `t`
succeed by defining the required implicit parameters.

There is one final tweak to make this all work: When using implicit parameters
for nested functions it was so far important to give all implicit parameters
of the same type the same name, or else one would get ambiguities. For instance, consider the
following fragment:
```scala
def f(implicit c: C) = {
  def g(implicit c: C) = ... implicitly[C] ...
  ...
}
```
If we had named the inner parameter `d` instead of `c` we would
have gotten an implicit ambiguity at the call of `implicitly` because
both `c` and `d` would be eligible:
```scala
def f(implicit c: C) = {
  def g(implicit d: C) = ... implicitly[C] ... // error!
  ...
}
```
The problem is that parameters in implicit closures now have
compiler-generated names, so the programmer cannot enforce the proper
naming scheme to avoid all ambiguities. We fix the problem by
introducing a new disambiguation rule which makes nested occurrences
of an implicit take precedence over outer ones. This rule, which
applies to all implicit parameters and implicit locals, is conceptually
analogous to the rule that prefers implicits defined in companion
objects of subclasses over those defined in companion objects of
superclass. With that new disambiguation rule the example code above
now compiles.

That's the complete set of rules needed to deal with implicit function types.

## How to Remove Boilerplate

The main advantage of implicit function types is that, being types,
they can be abstracted. That is, one can define a name for an implicit
function type and then use just the name instead of the full type.
Let's revisit our previous example and see how it can be made more
concise using this technique.

We first define a type `Transactional` for functions that take an implicit parameter of type `Transaction`:
```scala
type Transactional[T] = implicit Transaction => T
```
Making the return type of `f1` to `f3` a `Transactional[Int]`, we can
eliminate their implicit parameter sections:
```scala
  def f1(x: Int): Transactional[Int] = {
    thisTransaction.println(s"first step: $x")
    f2(x + 1)
  }
  def f2(x: Int): Transactional[Int] = {
    thisTransaction.println(s"second step: $x")
    f3(x * x)
  }
  def f3(x: Int): Transactional[Int] = {
    thisTransaction.println(s"third step: $x")
    if (x % 2 != 0) thisTransaction.abort()
    x
  }
```
You might ask, how does `thisTransaction` typecheck, since there is no
longer a parameter with that name? In fact, `thisTransaction` is now a
global definition:
```scala
  def thisTransaction: Transactional[Transaction] = implicitly[Transaction]
```
You might ask: a `Transactional[Transaction]`, is that not circular? To see more clearly, let's expand
the definition according to the rules given in the last section. `thisTransaction`
is of implicit function type, so the right hand side is expanded to the
implicit closure
```scala
  implicit ($ev0: Transaction) => implicitly[Transaction]
```
The right hand side of this closure, `implicitly[Transaction]`, needs
an implicit parameter of type `Transaction`, so the closure is further
expanded to
```scala
  implicit ($ev0: Transaction) => implicitly[Transaction]($ev0)
```
Now, `implicitly` is defined in `scala.Predef` like this:
```scala
  def implicitly[T](implicit x: T) = x
```
If we plug that definition into the closure above and simplify, we get:
```scala
  implicit ($ev0: Transaction) => $ev0
```
So, `thisTransaction` is just the implicit identity function on `transaction`!
In other words, if we use `thisTransaction` in the body of `f1` to `f3`, it will
pick up and return the unnamed implicit parameter that's in scope.

Finally, here are the `transaction` and `main` method that complete
the example.  Since `transactional`'s parameter `op` is now a
`Transactional`, we can eliminate the `Transaction` argument to `op`
and the `Transaction` lambda in `main`; both will be added by the compiler.
```scala
  def transaction[T](op: Transactional[T]) = {
    implicit val trans: Transaction = new Transaction
    op
    trans.commit()
  }
  def main(args: Array[String]) = {
    transaction {
      val res = f1(args.length)
      println(if (thisTransaction.isAborted) "aborted" else s"result: $res")
    }
  }
```
## Categorically Speaking

There are many interesting connections with category theory to explore
here. On the one hand, implicit functions are used for tasks that are
sometimes covered with monads such as the reader monad. There's an
argument to be made that implicits have better composability than
monads and why that is.

On the other hand, it turns out that implicit functions can also be
given a co-monadic interpretation, and the interplay between monads and
comonads is very interesting in its own right.

But these discussions will have to wait for another time, as
this blog post is already too long.

## Conclusion

Implicit function types are unique way to abstract over the context in
which some piece of code is run. I believe they will deeply influence
the way we write Scala in the future. They are very powerful
abstractions, in the sense that just declaring a type of a function
will inject certain implicit values into the scope of the function's
implementation. Can this be abused, making code more obscure?
Absolutely, like every other powerful abstraction technique. To keep
your code sane, please keep the [Principle of Least Power](http://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html) in mind.
