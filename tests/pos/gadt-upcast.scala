trait TagA[A]
trait TagB[B]
trait BiTag[A, B] extends TagA[A] with TagB[B]
class IntStrTag extends TagA[Int] with TagB[String]

def biget[A, B]: BiTag[A, B] => (A, B) = {
 case _: IntStrTag => (0, "")
}

@main def app: Unit = {
  val ret = biget(new IntStrTag with BiTag[Int, String])
  println(ret)
}

