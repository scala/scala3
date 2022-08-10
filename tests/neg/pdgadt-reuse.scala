object test {
  trait Result[A]

  def cached[A](f: (x: A) => Result[x.type], toCache: A): (x: A) => Result[x.type] = {
    val cachedRes: Result[toCache.type] = f(toCache)
    def resFunc(x: A): Result[x.type] = {
      x match {
        case r: toCache.type => cachedRes
        case _ =>
          val res: Result[x.type] = cachedRes  // error
          f(x)
      }
    }
    resFunc
  }
}
