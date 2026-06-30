package dotty.vendored
package sourcecode


abstract class SourceValue[T]{
  def value: T
}
abstract class SourceCompanion[T, V <: SourceValue[T]](build: T => V){
  def apply()(implicit s: V): T = s.value
  implicit def wrap(s: T): V = build(s)
}
case class Name(value: String) extends SourceValue[String]
object Name extends SourceCompanion[String, Name](new Name(_)) with NameMacros {
  case class Machine(value: String) extends SourceValue[String]
  object Machine extends SourceCompanion[String, Machine](new Machine(_)) with NameMachineMacros
}
case class FullName(value: String) extends SourceValue[String]
object FullName extends SourceCompanion[String, FullName](new FullName(_)) with FullNameMacros {
  case class Machine(value: String) extends SourceValue[String]
  object Machine extends SourceCompanion[String, Machine](new Machine(_)) with FullNameMachineMacros
}

case class File(value: String) extends SourceValue[String]
object File extends SourceCompanion[String, File](new File(_)) with FileMacros

case class FileName(value: String) extends SourceValue[String]
object FileName extends SourceCompanion[String, FileName](new FileName(_)) with FileNameMacros

case class Line(value: Int) extends SourceValue[Int]
object Line extends SourceCompanion[Int, Line](new Line(_)) with LineMacros
case class Enclosing(value: String) extends SourceValue[String]

object Enclosing extends SourceCompanion[String, Enclosing](new Enclosing(_)) with EnclosingMacros {
  case class Machine(value: String) extends SourceValue[String]
  object Machine extends SourceCompanion[String, Machine](new Machine(_)) with EnclosingMachineMacros
}


case class Pkg(value: String) extends SourceValue[String]
object Pkg extends SourceCompanion[String, Pkg](new Pkg(_)) with PkgMacros

case class Text[T](value: T, source: String)
object Text extends TextMacros

case class Args(value: Seq[Seq[Text[?]]]) extends SourceValue[Seq[Seq[Text[?]]]]
object Args extends SourceCompanion[Seq[Seq[Text[?]]], Args](new Args(_)) with ArgsMacros
