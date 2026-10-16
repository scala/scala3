//> using options -Yexplicit-nulls

import language.experimental.errorHandling


val x: String ? String = null // error

val y: String ? Null = null // ok

val y2: String ? Unit = null // error

def foo[E](x: String ? E): String ? E = x

val z = foo(null)
val _: String? = z // Null is inferred
