object Test:
  def main(args: Array[String]): Unit =
    val a = Short.MinValue to (Short.MinValue + 1000)
    val b = (Short.MaxValue - 1000) to Short.MaxValue
    a: Range.Inclusive // type inference test
    b: Range.Inclusive // type inference test
    assert(a.head == Short.MinValue.toInt && a.last == (Short.MinValue + 1000))
    assert(b.head == (Short.MaxValue - 1000) && b.last == Short.MaxValue.toInt)

    val r = ((Short.MinValue to (Short.MinValue + 1000)) ++
            ((Short.MaxValue - 1000) to Short.MaxValue)).toArray
    r: Array[Int] // type inference test
    assert(r.head == Short.MinValue.toInt)
    assert(r.last == Short.MaxValue.toInt)
end Test
