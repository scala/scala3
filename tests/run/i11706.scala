// https://github.com/scala/scala3/issues/11706
import scala.compiletime.erasedValue

object Obj:

  inline def length[Tuple]: Int = loop[Tuple]

  private inline def loop[Tuple]: Int =
    inline erasedValue[Tuple] match
      case _: EmptyTuple     => 0
      case _: (head *: tail) => 1 + loop[tail]

end Obj

// Same code, but in a trait instead of an object
trait Trait:

  inline def length[Tuple]: Int = loop[Tuple]

  private inline final def loop[Tuple]: Int =
    inline erasedValue[Tuple] match
      case _: EmptyTuple     => 0
      case _: (head *: tail) => 1 + loop[tail]

end Trait

@main def Test() =
  println(Obj.length[(Int, Int, String)]) // OK
  new Trait:
    println(length[(Int, Int, String)])
