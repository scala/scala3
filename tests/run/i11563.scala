import scala.compiletime.{constValue, erasedValue, summonInline}
import scala.deriving.Mirror

trait Printer[T]:
  def format: String

given Printer[String]:
  def format: String = "String"

inline given[T](using mirror: Mirror.ProductOf[T]): Printer[T] = Printer.derived[T]

object Printer:
  inline def apply[T](using printer: Printer[T]): Printer[T] = printer

  inline def derived[T](using mirror: Mirror.ProductOf[T]): Printer[T] =
    val params = summonPrinters[mirror.MirroredElemTypes]
    new Printer[T] :
      def format: String = params.map(p => p.format).mkString(",")

inline def summonPrinters[Types <: Tuple]: Seq[Printer[?]] = inline erasedValue[Types] match
  case _: EmptyTuple => Seq.empty
  case _: (v *: vs) => summonInline[Printer[v]] +: summonPrinters[vs]

case class Simple(name: String)

object Test:
  def main(args: Array[String]): Unit =
    assert(Printer[String].format == "String")
    assert(Printer[Simple].format == "String")
