package tests.selftypes

class A:
  def a: Unit = ???

class HasASelfType:
  self: A =>
  def b: Unit = ???
