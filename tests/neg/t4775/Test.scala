class Test {
  import JavaClass._
  // in Scala2, this is not actually an error (see https://github.com/scala/bug/issues/4775)
  // overloading selection needs to decide between:
  //
  // def foo[T <: Element](a: Element, b: Class[T], c: Boolean, d: Class[_ <: T]*)
  // def foo[T <: Element](a: Element, b: Class[_ <: T], c: Boolean)
  //
  // The first is not as specific as the second, since it has more arguments.
  // The second _should_ be as specific as the first,
  // but the T parameter would need to capture the wildcard of b.
  // Since we don't allow wildcard capture anymore,
  // we cannot infer anything for T
  // and just say the second is not as specific as the first either.
  //
  // As a note to whomever will be revisiting this in the future,
  // we can select the same overload as Scala2 by selecting the one
  // without vararg parameter when neither is more specific.
  // The argument is that since one overload has varargs and the other doesn't,
  // the callsite must be trying to pass no varargs,
  // so we should select the overload that doesn't actually have any.
  // This breaks no tests, but potentially allows too much.
  foo(new Element, classOf[Element], false) // error
}
