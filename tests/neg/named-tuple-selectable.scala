class FromFields extends Selectable:
  type Fields = (i: Int)
  def selectDynamic(key: String) =
    List(1, 2, 3)

trait FromRefs extends Selectable:
  def selectDynamic(key: String) =
    List(1, 2, 3)

def test(
    fromFlds: FromFields,
    fromRefs: FromRefs { val i: Int }
): Unit =
  fromFlds.i(0) // error
  fromRefs.i(0) // error

  fromFlds.i.apply(0) // error
  fromRefs.i.apply(0) // error

  fromFlds.i[Int](List(1)) // error
  fromRefs.i[Int](List(1)) // error

  fromFlds.i(List(1)) // error
  fromRefs.i(List(1)) // error

  fromFlds.i.apply(List(1)) // error
  fromRefs.i.apply(List(1)) // error
