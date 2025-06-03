import scala.language.experimental.modularity
import scala.language.future

abstract class Vec:
  tracked val size: Int

@main def main =
  val v = new Vec:
    val size0: size.type = 10
    val size = 10
    val size1: size.type = 10
