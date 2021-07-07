package ext

extension (s: String)
  def foo: Int = 42
  def #*# (i: Int): (String, Int) = (s, i)

val a = "asd".foo

val c = "foo" #*# 23

trait Read[+T]:
  def fromString(s: String): Option[T]

extension (s: String)
  def readInto[T](using Read[T]): Option[T] = summon[Read[T]].fromString(s)
