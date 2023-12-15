

class Item(x: String)

inline given a: Conversion[String, Item] =
  Item(_)  // warn

inline given b: Conversion[String, Item] =
  (x => Item(x))  // warn

inline given c: Conversion[String, Item] =
  { x => Item(x) }  // warn

inline given d: Conversion[String, Item] with
  def apply(x: String) = Item(x) // ok