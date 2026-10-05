//> using options -Yexplicit-nulls

import language.experimental.errorHandling


val x: String ? String = null // error

val y: String ? Unit = null // ok

def foo[E](x: String ? E): String ? E = x

val z = foo(null)
val _: String? = z // Unit is inferred
