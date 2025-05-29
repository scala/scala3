
import util.Try

@main def Test =
  assert(Try(classOf[Leak].getDeclaredField("l")).isFailure)
  assert(classOf[Leak].getFields.length == 0)
  //classOf[Leak].getFields.map(_.getName).foreach(println) //DEBUG
  assert(classOf[C].getFields.length == 0)

class C:
  private val x = 42
  println(x)
  println(List(27).map(_ + x))
