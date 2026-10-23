// A field that is not a constant, so the interface has a static initializer
interface JavaStatic<A> {
  Object O = new Object();
  A apply(A x);
  default A twice(A x) { return apply(apply(x)); }
}
