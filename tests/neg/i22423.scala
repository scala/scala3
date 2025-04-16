//> using options -Xmax-inlines:7
import scala.deriving.Mirror
import scala.compiletime._
import scala.compiletime.ops.int._

object HintsAwareConfigReaderDerivation {
  inline def deriveReader[A]: ConfigReader[A] =
    readCaseClass()
    ???

  private inline def summonConfigReader[A]: ConfigReader[A] =
    summonFrom { case reader: ConfigReader[A] => reader }

  private inline def readCaseClass(): Unit =
    summonConfigReader[List[String]]
    val a1: Int = ???
    val a2: EmptyTuple = ???
    a1 *: a2
    ???
}

trait ConfigReader[A]
object ConfigReader {
  implicit def traversableReader[A, F[A] <: TraversableOnce[A]](implicit configConvert: ConfigReader[A]): ConfigReader[F[A]] = ???
  implicit def exportedReader[A](implicit exported: Exported[ConfigReader[A]]): ConfigReader[A] = exported.instance
  case class Exported[A](instance: A)
}

import ConfigReader._
inline given exportReader[A]: Exported[ConfigReader[A]] = Exported(HintsAwareConfigReaderDerivation.deriveReader[A])

case class Settings(rules: List[String])

val settings =
  exportReader[Settings] // error
