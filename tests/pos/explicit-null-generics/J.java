
class I<T> {}

class J {
  <T> I<T> foo(T x) {
  	return new I<T>();
  }

  <T> ReturnedFromJava<T> foo2(T x) {
 	return new ReturnedFromJava<T>();
  }
}
