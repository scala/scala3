// Test that `@NullMarked` / `@NullUnmarked` are read from class files.

import org.jspecify.annotations.J_2

// `get` result is non-null in a `@NullMarked` class.
def a(j: J_2): String = j.get()

// Type-use `@Nullable` still makes the result `String | Null`.
def b(j: J_2): String = j.getNullable() // error

// `set` parameter is non-null, so `null` is rejected.
def c(j: J_2): Unit = j.set(null) // error

// `@NullUnmarked` restores implicit nullability, so `null` is accepted here.
def d(j: J_2): Unit = j.unmarkedSet(null)
