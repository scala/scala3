class MyClass {
  @MyAnnotation(Name="Blah", foBaskaLi = Array(1,2,3))
  def method = ???
}

class MyClass2 extends AnyRef @MyAnnotation(Name="Foo", foBaskaLi = Array(1,2,3))

class MyClass3 extends AnyRef @MyAnnotation(Name="Foo", foBaskaLi = Array(1,2,3)) {
  @MyAnnotation(Name="Blah", foBaskaLi = Array(1,2,3))
  def method = ???
}
