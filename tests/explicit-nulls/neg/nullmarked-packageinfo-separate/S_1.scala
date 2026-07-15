// Test that a package-level `@NullMarked` (from `package-info.class`) is read from class files.
// The Java sources above are compiled first (round 0); this file is compiled separately and reads
// `nmpkg.J` through ClassfileParser.
//> using options -Yno-flexible-types

import nmpkg.J

// `get` result is non-null because the package is `@NullMarked`.
def a(j: J): String = j.get()

// Type-use `@Nullable` still makes the result `String | Null`.
def b(j: J): String = j.getNullable() // error

// `set` parameter is non-null, so `null` is rejected.
def c(j: J): Unit = j.set(null) // error

// `@NullUnmarked` on the method overrides the package marking, so `null` is accepted here.
def d(j: J): Unit = j.unmarkedSet(null)
