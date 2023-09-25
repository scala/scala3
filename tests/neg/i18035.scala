import reflect.Selectable.reflectiveSelectable

class A[+Cov](f: Cov => Unit) {
  def foo: { def apply(c: Cov): Unit } = // error
    f
}

val aForString = new A[String](_.length)
// => val aForString: A[String]

val aForStringIsAForAny: A[Any] = aForString
// => val aForStringIsAForAny: A[Any]

val _ = aForStringIsAForAny.foo(123)
// => java.lang.ClassCastException: class java.lang.Integer cannot be cast to class java.lang.String (java.lang.Integer and java.lang.String are in module java.base of loader 'bootstrap')
