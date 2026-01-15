trait Buffer[A]:

  def apply(i: Int): A = ???

  def flatMapInPlace(f: A => IterableOnce[A]^): this.type = {
    val g = f
    val s = 10
     // capture checking: we need the copy since we box/unbox on g* on the next line
      // TODO: This looks fishy, need to investigate
      // Alternative would be to mark `f` as @use. It's not inferred
      // since `^ appears in a function result, not under a box.
    val newElems = new Array[(IterableOnce[A]^{f})](s)
    var i = 0
    while i < s do
      val x = g(this(i))
      newElems(i) = x
      i += 1
    this
  }