
object Test {

  def main(args: Array[String]): Unit = {
    assert(null.eq(null))
    assert(!null.ne(null))

    assert(!null.eq("hello"))
    assert(null.ne("hello"))

    assert(!null.eq(4))
    assert(null.ne(4))

    assert(!"hello".eq(null))
    assert("hello".ne(null))

    assert(!4.eq(null))
    assert(4.ne(null))

    val x: String|Null = null
    assert(x.eq(null))
    assert(!x.ne(null))

    val x2: AnyRef|Null = "world"
    assert(!x2.eq(null))
    assert(x2.ne(null))
  }
}
