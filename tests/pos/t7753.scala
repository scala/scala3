import scala.language.{ higherKinds, implicitConversions }

trait Foo { type Out }

trait SI {
  val inst: Foo
  type Out
}

object Test {
  def test: Unit = {
    def indirect(si: SI)(v: si.inst.Out) = v

    val foo: Foo { type Out = Int } = ???
    def conv(i: Foo): SI { type Out = i.Out; val inst: i.type } = ???

    val converted = conv(foo)

    val v1: Int = indirect(converted)(23)  // Okay (after refining the return type `inst` in the return type of `conv`)
    /*
    indirect(converted){(v: converted.inst.Out)converted.inst.Out}(
      23{Int(23)}
    ){converted.inst.Out};
    */

    val v2: Int = indirect(conv(foo))(23)  // Used to fail as follows:
    /*
    indirect(
        conv(foo){si.SI{type Out = foo.Out; val inst: si.Test.<refinement>.type}}
    ){(v: si.inst.Out)si.inst.Out}(
      23{<error>}
    ){<error>};
    */

  }
}
