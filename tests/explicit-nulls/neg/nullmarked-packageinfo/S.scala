//> using options -Yno-flexible-types

import nmpkg.J
import nmpkg.U

// `get` result is non-null because the package is `@NullMarked`.
def a(j: J): String = j.get()

// Type-use `@Nullable` still makes the result `String | Null`.
def b(j: J): String = j.getNullable() // error

// `set` parameter is non-null, so `null` is rejected.
def c(j: J): Unit = j.set(null) // error

// `@NullUnmarked` on the method overrides the package marking, so `null` is accepted here.
def d(j: J): Unit = j.unmarkedSet(null)

// `@NullUnmarked` on class `U` overrides the package `@NullMarked`: `get` is `String | Null`.
def u1(u: U): String = u.get() // error

// `@NullUnmarked` class: `set` parameter is nullable, so `null` is accepted.
def u2(u: U): Unit = u.set(null)

// `@NullMarked` re-marks the method inside the unmarked class, so its result is non-null.
def u3(u: U): String = u.markedGet()

// Both markers behave as if neither: `bothGet` inherits `U`'s `@NullUnmarked`, so its result is
// `String | Null` (it does not become non-null from `@NullMarked`).
def u4(u: U): String = u.bothGet() // error
