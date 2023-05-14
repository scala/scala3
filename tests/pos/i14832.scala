class Box[V](val value: V)

class Test:
  def value: Box["text"] = Box("text")

  def test: String = value match
    case b: Box[_ <: String] => b.value
