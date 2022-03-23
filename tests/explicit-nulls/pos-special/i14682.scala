class C1:
  sealed abstract class Name {
    type ThisName <: Name
    def compareTo(that: ThisName): Int = ???
  }

  class LocalName extends Name with Comparable[LocalName] {
    type ThisName = LocalName
  }

  val localName = LocalName()
  println(localName)
  var count = 0

class C2:
  sealed abstract class Name {
    type ThisName <: Name
    def compareTo(that: ThisName | Null): Int = ???
  }

  class LocalName extends Name with Comparable[LocalName] {
    type ThisName = LocalName
  }

  val localName = LocalName()
  println(localName)
  var count = 0