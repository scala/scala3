trait TagA[A]
trait TagB[B]
trait TagC[C]
trait TriTag[A, B, C] extends TagA[A] with TagB[B] with TagC[C]
class IntStrCharTag extends TagA[Int] with TagB[String] with TagC[Char]

def get[A, B, C]: TriTag[A, B, C] => (A, B, C) = {
 case _: IntStrCharTag => (0, "zero", '0')
}

object GadtUpcast extends App {
  val ret = get(new IntStrCharTag with TriTag[Int, String, Char])
  println(ret)
}
