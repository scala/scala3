
import scala.quoted.*
import Macro.*

object Macro2 {
  // TODO should elems of `new Record` and `Record.fromUntypedTuple` be IArray[Object]
  // This would make it possible to keep the same reference to the elements when transforming a Tuple into a Record (or vice versa)

  case class Record(elems: (String, Any)*) extends SelectableRecord {
    def selectDynamic(name: String): Any = elems.find(_._1 == name).get._2
    override def toString(): String = elems.map(x => x._1 + "=" + x._2).mkString("Record(", ", ", ")")
  }

  object Record extends SelectableRecordCompanion[Record] {
    import scala.quoted.*

    inline def apply[R <: Record](elems: (String, Any)*) : R = ${ applyImpl[R]('elems) }

    def applyImpl[R <: Record: Type](elems: Expr[Seq[(String, Any)]])(using Quotes) = {
      '{ new Record($elems:_*).asInstanceOf[R] }
    }

    def fromUntypedTuple(elems: (String, Any)*): Record = new Record(elems*)
      // `new` is needed since resolving the two `apply`s is ambiguous; this was hidden by old scheme for creator applications
  }
}