class T

enum Foo[T](val foo: Any) {
  case Case1(x: Int) extends Foo(new T)
  case Case2[U](x: U) extends Foo(new T)
  case Case3 extends Foo(new T)
}
