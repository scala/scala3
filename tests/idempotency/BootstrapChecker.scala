import java.nio.file.{Files, Path, Paths}
import java.util.stream.{ Stream => JStream }

import scala.collection.JavaConverters._

object Test {
  def main(args: Array[String]): Unit = IdempotencyCheck.checkIdempotency("../out/dotty")
}
