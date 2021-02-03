class S {
  val j = new J()
  val x: Array[Dog] = ???
  // Check that the java varargs for `foo` gets typed as `Array[_ <: Animal]`.
  // Otherwise, the call below would fail in -Ycheck:elimRepeated because arrays are invariant before erasure.
  // This is unsound but allowed.
  j.foo(x*)
  j.foo(new Dog, new Dog)
}
