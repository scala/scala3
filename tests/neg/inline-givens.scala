
class Item(x: String)

inline given a: Conversion[String, Item] =
  Item(_)  // error

inline given b: Conversion[String, Item] with
  def apply(x: String) = Item(x)

