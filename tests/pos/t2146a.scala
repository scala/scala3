object nestedImplicits {

  trait A
  trait B

  def foo: implicit A => implicit B => Int = {
    implicitly[A]
    implicitly[B]
    42
  }

  foo(new A{})(new B{})
}