import scala.compiletime.*
import scala.language.dynamics

abstract class % extends Selectable

trait Select { type Out <: % }
trait Selector extends Dynamic {
  def selectDynamic[S <: Singleton & String](label: S): Any = ???

  def unapply[R: RecordLike](record: R)(using
      t: Select,
      r: RecordLike[t.Out]
  ): r.ElemTypes = ???
}

trait RecordLike[R] {
  type ElemTypes <: Tuple
}


@main def Test = {
  val r: %{ val name: String; } = ???

  // originally derived in macro, use dummy instance instead
  transparent inline given outputRecordLike: [R <: %] => RecordLike[R] = null.asInstanceOf[
    RecordLike[R] {
      type ElemTypes = String *: EmptyTuple
    }
  ]

  type FieldSelector = Select { type Out = % { val name: String } }
  given fieldSelector: FieldSelector = ???
  val selector: Selector = ???

  val works = selector.unapply(r)
  val works2 = selector.unapply(r)(using summon, fieldSelector, summon)
  r match {
    case selector(value) => value // compilation error
  }
}
