
class I<T> {}

class J {
  <T> I<T> foo(T x) {
  	return new I<T>();
  }
  // TODO(abeln): test returning a Scala generic from Java
}
