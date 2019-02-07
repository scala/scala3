---
layout: doc-page
title: Inline
---

`inline` is a new [soft modifier](../soft-modifier.html) that guarantees that a definition will be inline at the point of use. Example:

    object Config {
      inline val logging = false
    }

    object Logger {

      private var indent = 0

      inline def log[T](msg: => String)(op: => T): T =
        if (Config.logging) {
          println(s"${"  " * indent}start $msg")
          indent += 1
          val result = op
          indent -= 1
          println(s"${"  " * indent}$msg = $result")
          result
        }
        else op
    }

The `Config` object contains a definition of a `inline` value
`logging`. This means that `logging` is treated as a constant value,
equivalent to its right-hand side `false`. The right-hand side of such
a inline val must itself be a [constant
expression](#the-definition-of-constant-expression). Used in this way,
`inline` is equivalent to Java and Scala 2's `final`. `final` meaning
"constant" is still supported in Dotty, but will be phased out.

The `Logger` object contains a definition of an `inline` method `log`.
This method will always be inlined at the point of call.

In the inlined code, an if-then-else with a constant condition will be
rewritten to its then- or else-part. Here's an example:

    def factorial(n: BigInt): BigInt =
      log(s"factorial($n)") {
        if (n == 0) 1
        else n * factorial(n - 1)
      }

If `Config.logging == false`, this will be rewritten to

    def factorial(n: BigInt): BigInt = {
      def msg = s"factorial($n)"
      def op =
        if (n == 0) 1
        else n * factorial(n - 1)
      op
  }

Note that the arguments corresponding to the parameters `msg` and `op`
of the inline method `log` are defined before the inlined body (which
is in this case simply `op`). By-name parameters of the inline method
correspond to `def` bindings whereas by-value parameters correspond to
`val` bindings. So if `log` was defined like this:

    inline def log[T](msg: String)(op: => T): T = ...

we'd get

    val msg = s"factorial($n)"

instead. This behavior is designed so that calling an inline method is
semantically the same as calling a normal method: By-value arguments
are evaluated before the call whereas by-name arguments are evaluated
each time they are referenced. As a consequence, it is often
preferable to make arguments of inline methods by-name in order to
avoid unnecessary evaluations.

For instance, here is how we can define a zero-overhead `foreach` method
that translates into a straightforward while loop without any indirection or
overhead:

    inline def foreach(op: => Int => Unit): Unit = {
      var i = from
      while (i < end) {
        op(i)
        i += 1
      }
    }

By contrast, if `op` is a call-by-value parameter, it would be evaluated separately as a closure.

Inline methods can be recursive. For instance, when called with a constant
exponent `n`, the following method for `power` will be implemented by
straight inline code without any loop or recursion.

    inline def power(x: Double, n: Int): Double =
      if (n == 0) 1.0
      else if (n == 1) x
      else {
        val y = power(x, n / 2)
        if (n % 2 == 0) y * y else y * y * x
      }

      power(expr, 10)
        // translates to
        //
        //    val x = expr
        //    val y1 = x * x   // ^2
        //    val y2 = y1 * y1 // ^4
        //    val y3 = y2 * x  // ^5
        //    y3 * y3          // ^10

Parameters of inline methods can be marked `inline`. This means
that actual arguments to these parameters must be constant expressions.

### Relationship to `@inline`.

Scala also defines a `@inline` annotation which is used as a hint
for the backend to inline. The `inline` modifier is a more powerful
option: Expansion is guaranteed instead of best effort,
it happens in the frontend instead of in the backend, and it also applies
to recursive methods.

To cross compile between both Dotty and Scalac, we introduce a new `@forceInline`
annotation which is equivalent to the new `inline` modifier. Note that
Scala 2 ignores the `@forceInline` annotation, so one must use both
annotations to guarantee inlining for Dotty and at the same time hint inlining
for Scala 2 (i.e. `@forceInline @inline`).

### The definition of constant expression

Right-hand sides of inline values and of arguments for inline parameters
must be constant expressions in the sense defined by the [SLS §
6.24](https://www.scala-lang.org/files/archive/spec/2.12/06-expressions.html#constant-expressions),
including "platform-specific" extensions such as constant folding of
pure numeric computations.

### Reference

For more info, see [PR #4927](https://github.com/lampepfl/dotty/pull/4768), which explains how
inline methods can be used for typelevel programming and code specialization.
