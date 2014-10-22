object Test1672 {
  @annotation.tailrec
  def bar(x: Int)(y: Int) : Nothing = {
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar(x)(y)
    }
  }
}
