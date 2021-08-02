package tests.selftypes

class A:
  def a: Unit = ???

class C

class HasAnAndSelfType:
  self: A & C =>
  def b: Unit = ???

class HasASelfType:
  self: A =>
  def b: Unit = ???
