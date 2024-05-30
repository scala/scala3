import scala.language.experimental.namedTuples

class FromFields extends Selectable:
  type Fields = (xs: List[Int], poly: [T] => (x: List[T]) => Option[T])
  def selectDynamic(key: String) =
    List(1, 2, 3)

trait FromRefs extends Selectable:
  def selectDynamic(key: String) =
    List(1, 2, 3)

def test(
    fromFlds: FromFields,
    fromRefs: FromRefs { val xs: List[Int]; val poly: [T] => (x: List[T]) => Option[T] }
): Unit =
  fromFlds.xs(0)
  fromRefs.xs(0)

  fromFlds.xs.apply(0)
  fromRefs.xs.apply(0)

  fromFlds.poly[Int](List(1)): Option[Int]
  fromRefs.poly[Int](List(1)): Option[Int]

  fromFlds.poly(List(1))
  fromRefs.poly(List(1))

  fromFlds.poly.apply(List(1))
  fromRefs.poly.apply(List(1))
