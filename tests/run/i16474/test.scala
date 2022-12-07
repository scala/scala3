// scalajs: --skip
// test.scala
import repro.*
import scala.util.Try

def get(arg: Any): Try[Encrypter] = Try {
  val x: Any = 1
  arg match
    case 1 => new Case1()
    case 2 => new Case2()
    case _ => throw new RuntimeException(s"Unsupported got $arg")
}

@main def Test =
    val result = get(null)
