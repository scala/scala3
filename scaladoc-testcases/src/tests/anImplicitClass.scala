package tests.anImplicitClass

implicit class AddingOps(n: Int):
  def inc: Int = n + 1
