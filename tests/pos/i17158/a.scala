package foo

opaque type Headers = Array[String]

extension (headers: Headers)
  def foreach(f: (String, String) => Unit): Unit =
    ???
