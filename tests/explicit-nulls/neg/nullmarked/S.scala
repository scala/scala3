import nullmarked.J

// `get` result is non-null in a `@NullMarked` class.
def a(j: J): String = j.get()

// Type-use `@Nullable` still makes the result `String | Null`.
def b(j: J): String = j.getNullable() // error

// `set` parameter is non-null, so `null` is rejected.
def c(j: J): Unit = j.set(null) // error

// `@NullUnmarked` restores implicit nullability, so `null` is accepted here.
def d(j: J): Unit = j.unmarkedSet(null)
