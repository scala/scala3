package ext

extension (s: String)
  def :*: (i: Int): (String, Int) = (s, i)

val b = "foo" :*: 23