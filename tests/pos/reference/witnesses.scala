class Common {

  trait Ord[T] {
    def compareTo(this x: T)(y: T): Int
    def < (this x: T)(y: T) = x.compareTo(y) < 0
    def > (this x: T)(y: T) = x.compareTo(y) > 0
  }

  trait Convertible[From, To] {
    def convert (this x: From): To
  }

}

object Implicits extends Common {
  implicit object IntOrd extends Ord[Int] {
    def compareTo(this x: Int)(y: Int) =
      if (x < y) -1 else if (x > y) +1 else 0
  }

  class ListOrd[T: Ord] extends Ord[List[T]] {
    def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs1, y :: ys1) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs1.compareTo(ys1)
    }
  }
  implicit def ListOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]

  class Convertible_List_List_witness[From, To](implicit c: Convertible[From, To])
  extends Convertible[List[From], List[To]] {
    def convert (this x: List[From]): List[To] = x.map(c.convert)
  }
  implicit def Convertible_List_List_witness[From, To](implicit c: Convertible[From, To])
    : Convertible[List[From], List[To]] =
    new Convertible_List_List_witness[From, To]

  def maximum[T](xs: List[T])
                (implicit cmp: Ord[T]): T =
    xs.reduceLeft((x, y) => if (x < y) y else x)

  def descending[T](implicit asc: Ord[T]): Ord[T] = new Ord[T] {
    def compareTo(this x: T)(y: T) = asc.compareTo(y)(x)
  }

  def minimum[T](xs: List[T])(implicit cmp: Ord[T]) =
    maximum(xs)(descending)
}