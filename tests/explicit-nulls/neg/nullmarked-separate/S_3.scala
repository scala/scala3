// Test that `@NullMarked` / `@NullUnmarked` are read from class files.
//> using options -Yno-flexible-types

import org.jspecify.annotations.J_2

// `get` result is non-null in a `@NullMarked` class.
def a(j: J_2): String = j.get()

// Type-use `@Nullable` still makes the result `String | Null`.
def b(j: J_2): String = j.getNullable() // error

// `set` parameter is non-null, so `null` is rejected.
def c(j: J_2): Unit = j.set(null) // error

// `@NullUnmarked` restores implicit nullability, so `null` is accepted here.
def d(j: J_2): Unit = j.unmarkedSet(null)

// Both markers on `bothSet` behave as if neither: it inherits the class `@NullMarked`, so the
// parameter is non-null and `null` is rejected.
def dd(j: J_2): Unit = j.bothSet(null) // error

// `@NullUnmarked` constructor: the parameter is nullable, so `null` is accepted.
def e: J_2 = new J_2(null)

// `@NullUnmarked` nested class: `innerGet` result is `String | Null` again.
def f(i: J_2.Inner): String = i.innerGet() // error

// `@NullUnmarked` nested class: `innerSet` parameter is nullable, so `null` is accepted.
def g(i: J_2.Inner): Unit = i.innerSet(null)

// `@NullMarked` re-marks inside the unmarked nested class, so this result is non-null.
def h(i: J_2.Inner): String = i.remarkedGet()
