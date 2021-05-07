sealed trait Base { def str: String }
case class One(str: String) extends Base
case class Two(str: String) extends Base
case class Three(str: String) extends Base

case class Item(_id: String)

private def doWithItem[T <: (One | Two | Three)]
  (item: Item, value: T, action: (T) => Item) = doWithItemId(item._id, value, action)
private def doWithItemId[U <: (One | Two | Three)]
  (itemId: String, value: U, action: (U) => Item) =
    println(value.str)
    Item("_id")