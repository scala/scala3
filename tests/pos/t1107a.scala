object F {
  implicit def stripNull[T](x: T | Null): T = x.asInstanceOf[T]
  type AnyClass = Class[_]
  def tryf[T](ignore: List[AnyClass] | Null)(f: => T): Any = {
    try {
      f
    } catch {
      case e if ignore == null || ignore.isEmpty => {false}
    }
  }
}
