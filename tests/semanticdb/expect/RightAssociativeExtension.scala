package ext

extension (i: Int)
  def :*: (s: String): (String, Int) = (s, i)

val b = "foo" :*: 23