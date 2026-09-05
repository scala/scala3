// https://github.com/scala/scala3/issues/18261
//
// Like tests/pos/i18261, but with a match type whose type parameter appears
// only in the case body, never in the scrutinee. Note that this compiles on
// its own; it is here as coverage for that shape, not as a reproduction.

trait VAL

type Foo[T, M] = M match
  case VAL => T
