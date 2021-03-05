package ext

extension (s: String)
  def foo: Int = 42
  def #*# (i: Int): (String, Int) = (s, i)

val a = "asd".foo

val c = "foo" #*# 23