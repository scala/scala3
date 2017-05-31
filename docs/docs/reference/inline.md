---
layout: doc-page
title: Inline
---

`inline` is a new modifier that guarantees that a definition will be
inlined at the point of use. Example:

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

The `Config` object contains a definition of an `inline` value
`logging`. This means that `logging` is treated as a constant value,
equivalent to its right-hand side `false`. The right-hand side of such
an inline val must itself be a constant expression. Used in this way,
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
is in this case simply `op`). By-name parameters of the inlined method
correspond to `def` bindings whereas by-value parameters correspond to
`val` bindings. So if `log` was defined like this:

    inline def log[T](msg: String)(op: => T): T = ...

we'd get

    val msg = s"factorial($n)"

instead. This behavior is designed so that calling an inline method is
semantically the same as calling a normal method: By-value arguments
are evaluated before the call wherea by-name arguments are evaluated
each time they are referenced. As a consequence, it is often
preferrable to make arguments of inline methods by-name in order to
avoid unnecessary evaluations.

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

Parameters of inline methods can themselves be marked `inline`. This means
that the argument of an inline method is itself inlined in the inlined body of
the method. Using this scheme, we can define a zero-overhead `foreach` method
that translates into a straightforward while loop without any indirection or
overhead:

    inline def foreach(inline op: Int => Unit): Unit = {
      var i = from
      while (i < end) {
        op(i)
        i += 1
      }
    }

### Relationship to `@inline`.

Existing Scala defines a `@inline` annotation which is used as a hint
for the backend to inline. For most purposes, this annotation is
superseded by the `inline` modifier. The modifier is more powerful
than the annotation: Expansion is guarenteed instead of best effort,
it happens in the fronend instead of in the backend, and it also applies
to method arguments and recursive methods.

Since `inline` is now a keyword, it would be a syntax error to write
`@inline`. However, one can still refer to the annotation by putting
it in backticks, i.e.

    @`inline` def ...


### Reference

For more info, see [PR #1492](https://github.com/lampepfl/dotty/pull/1492) and
[Scala SIP 28](http://docs.scala-lang.org/sips/pending/inline-meta.html)



