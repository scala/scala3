object Module:

  class Bar
  class Baz
  class Qux

  object Givens:
    given GivenBar: Bar = new Bar()
    def GivenBar(ignored: Int): Bar = new Bar()
    class GivenBar

  object Members:
    given Member: Baz = new Baz()
    private def Member(ignored1: String)(ignored2: Int): Bar = new Bar()
    def Member(ignored: Int): Baz = new Baz()
    class Member

  object Combined:
    given GivenQux: Qux = new Qux()
    def GivenQux(ignored: Int): Qux = new Qux()

  enum Color:
    case Red, Green, Blue

  export Color._ // will only export synthetic defs with same name as standard definition
  export Givens.given // should only export given values
  export Members._ // should only export values that are not given
  export Combined.{_, given} // should only export values that are not given

@main def Test =

  println(Module.Red)
  println(Module.valueOf("Red")) // error: value valueOf is not a member

  println(summon[Module.Bar])
  println(new Module.GivenBar()) // error: type GivenBar is not a member
  println(Module.GivenBar(23)) // error: method GivenBar does not take parameters

  println(new Module.Member())
  println(Module.Member(23))
  println(Module.Member("?")(23)) // error: Found: ("?" : String) Required: Int
  println(summon[Module.Baz]) // error: no implicit argument of type Module.Baz was found

  println(summon[Module.Qux])
  println(Module.GivenQux(23))
