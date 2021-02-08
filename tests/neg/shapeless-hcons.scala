package shapeless {

sealed trait HList extends Product with Serializable {
  def :: (x: Any): HList = new ::(x, this)
}

final case class ::[+H, +T <: HList](head : H, tail : T) extends HList {
  override def toString = head match {
    case _: ::[_, _] => s"($head) :: $tail"
    case _ => s"$head :: $tail"
  }
}

sealed trait HNil extends HList {
  override def toString = "HNil"
}

case object HNil extends HNil

}
import shapeless.*

package test {

object Test {

  val xs = 1 :: 2 :: Nil
  val ys = (3, 4)

  (xs: Any) match {
    case x :: xs => ???
  }
  xs match {
    case x :: xs => ???   // error: unreachable case
  }

}
}