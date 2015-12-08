import scala.collection._
import scala.collection.generic._

trait ViewMkString[+A]

trait TraversableViewLike[+A,
                          +Coll,
                          +This <: TraversableView[A, Coll] with TraversableViewLike[A, Coll, This]]
  extends Traversable[A] with TraversableLike[A, This] with ViewMkString[A] { self =>

  def f[B](pf: PartialFunction[A, B]) =
    filter(pf.isDefinedAt).map(pf)

}

trait TraversableView[+A, +Coll] extends TraversableViewLike[A, Coll, TraversableView[A, Coll]] { }


