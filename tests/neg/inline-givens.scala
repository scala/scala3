// scalac: -Xfatal-warnings

class Item(x: String)

inline given a: Conversion[String, Item] =
  Item(_)  // error

inline given b: Conversion[String, Item] =
  (x => Item(x))  // error

inline given c: Conversion[String, Item] =
  { x => Item(x) }  // error

inline given d: Conversion[String, Item] with
  def apply(x: String) = Item(x) // ok

