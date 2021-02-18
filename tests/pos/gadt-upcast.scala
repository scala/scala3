trait TagA[A]
trait TagB[B]
trait TagC[C]
trait ProxyTagC[C] extends TagC[C]
trait TriTag[A, B, C] extends TagA[A] with TagB[B] with ProxyTagC[C]
class IntStrCharTag extends TagA[Int] with TagB[String] with TagC[Char]

def biget[A, B, C]: TriTag[A, B, C] => (A, B, C) = {
 case _: IntStrCharTag => (0, "zero", '0')
}

object GadtUpcast extends App {
  val ret = biget(new IntStrCharTag with TriTag[Int, String, Char])
  println(ret)
}
