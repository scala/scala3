object TestenumS:
  def go() = println("Scala: Testme Hello= " + Testme.Hello)

enum Testme extends java.lang.Enum[Testme]:
  case Hello
