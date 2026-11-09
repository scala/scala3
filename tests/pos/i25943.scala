// https://github.com/scala/scala3/issues/25943
class C25943(a: Any)

def compilerThrows25943(): Unit =
  val a2 = new Object

  new :
    class S extends C25943(a2)
    new S{}

val outsideA25943 = new Object

def noThrow25943(): Unit =
  val a2 = new Object

  val _ =
    class S extends C25943(a2)
    new S {}

  new :
    class S extends C25943(a2)

    class S_outsideA extends C25943(outsideA25943)
    new S_outsideA {}

    new C25943(a2) {}
