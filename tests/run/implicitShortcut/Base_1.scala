package implicitShortcut

class C
abstract class Base[T] {

  def foo(x: T): given C => T = x

}