//> using options -Werror -Wunused:privates

case class BinaryFen(value: Array[Byte]) extends AnyVal:

  def read: Unit =
    val reader = new Iterator[Byte]:
      val inner                            = value.iterator
      override inline def hasNext: Boolean = inner.hasNext // nowarn
      override inline def next: Byte       = if hasNext then inner.next() else 0.toByte // nowarn

    if reader.hasNext then
      val b: Byte = reader.next()
      println(b)
