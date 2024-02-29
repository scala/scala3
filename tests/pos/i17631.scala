//> using options -Xfatal-warnings -Wunused:all -deprecation -feature

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
