
class Foo

extension (dfVal: Foo)
  def f0(step: Int): Foo = ???
  def f0: Foo = ???
val v0 = (new Foo).f0

extension (dfVal: Foo)
  def f1[T](step: Int): Foo = ???
  def f1: Foo = ???
val v1 = (new Foo).f1

extension (dfVal: Foo)
  def f2[T](step: Int): Foo = ???
  def f2[T]: Foo = ???
val v2 = (new Foo).f2

extension [A](dfVal: Foo)
  def f3[T](step: Int): Foo = ???
  def f3: Foo = ???
val v3 = (new Foo).f3

extension [A](dfVal: Foo)
  def f4[T](step: Int): Foo = ???
  def f4[T]: Foo = ???
val v4 = (new Foo).f4
