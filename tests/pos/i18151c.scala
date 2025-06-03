import scala.compiletime.*
import scala.compiletime.ops.any.ToString

trait Attr
case object EmptyAttr extends Attr
transparent inline def attrStr(inline a: Attr): String = inline a match
  case EmptyAttr => ""
transparent inline def attrStrHelper(inline a: Attr): String = inline a match
  case EmptyAttr => ""
trait TmplNode
case class El[T <: String & Singleton, A <: Attr, C <: Tmpl](val tag: T, val attr: A, val child: C)
    extends TmplNode
case class Sib[L <: Tmpl, R <: Tmpl](left: L, right: R) extends TmplNode
type TmplSingleton = String | Char | Int | Long | Float | Double | Boolean
type Tmpl = TmplNode | Unit | (TmplSingleton & Singleton)
transparent inline def tmplStr(inline t: Tmpl): String = inline t match
  case El(tag, attr, child) => inline attrStr(attr) match
      case "" => "<" + tag + ">" + tmplStr(child)
      case x  => "<" + tag + " " + x + ">" + tmplStr(child)
  case Sib(left, right) => inline tmplStr(right) match
      case ""    => tmplStr(left)
      case right => tmplStrHelper(left) + right
  case ()                     => ""
  case s: (t & TmplSingleton) => constValue[ToString[t]]
transparent inline def tmplStrHelper(inline t: Tmpl): String = inline t match
  case El(tag, attr, child) => inline (tmplStr(child), attrStr(attr)) match
      case ("", "")      => "<" + tag + "/>"
      case (child, "")   => "<" + tag + ">" + child + "</" + tag + ">"
      case ("", attr)    => "<" + tag + " " + attr + "/>"
      case (child, attr) => "<" + tag + " " + attr + ">" + child + "</" + tag + ">"
  case Sib(left, right)       => tmplStrHelper(left) + tmplStrHelper(right)
  case ()                     => ""
  case s: (t & TmplSingleton) => constValue[ToString[t]]
transparent inline def el(tag: String & Singleton): El[tag.type, EmptyAttr.type, Unit] =
  El(tag, EmptyAttr, ())
extension [T <: String & Singleton, A <: Attr, C <: Tmpl](el: El[T, A, C])
  transparent inline def >>[C2 <: Tmpl](child: C2) = El(el.tag, el.attr, el.child ++ child)

extension [L <: Tmpl](left: L) transparent inline def ++[R <: Tmpl](right: R) = Sib(left, right)
