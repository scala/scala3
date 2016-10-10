object Test {

  def foo(lis: scala.collection.immutable.Set[Int] | scala.collection.immutable.Set[String]) = lis
  foo(Set(1))
  foo(Set(""))
}
