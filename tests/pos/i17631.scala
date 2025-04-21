//> using options -Werror -Wunused:all -deprecation -feature

object foo {
  type Bar
}

import foo.Bar

def Test = {

  type Person = { val name: String }

  def good: Person = ???
  def bad1: { val name: String } = ???
  def bad2 = (good: { val name: String })
  def justIs: { val bar: Bar } = ???
  (bad1, bad2, justIs)
}

class Record(elems: (String, Any)*) extends Selectable:
  private val fields = elems.toMap
  def selectDynamic(name: String): Any = fields(name)

object Main {

  type Person = Record { val name: String; val age: Int }

  locally {
    def good: Person = ???
    def bad1: Record { val name: String; val age: Int } = ???
    def bad2 = (good: Record { val name: String; val age: Int })
    (bad1, bad2)
  }
}

def `i18388`: Unit =
  def func(pred: [A] => A => Boolean): Unit =
    val _ = pred
    ()
  val _ = func

trait L[T]:
  type E

def `i19748` =
  type Warn1 = [T] => (l: L[T]) => T => l.E
  type Warn2 = [T] => L[T] => T
  type Warn3 = [T] => T => T
  def use(x: (Warn1, Warn2, Warn3)) = x
  use

type NoWarning1 = [T] => (l: L[T]) => T => l.E
type NoWarning2 = [T] => L[T] => T
type NoWarning3 = [T] => T => T
