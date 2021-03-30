class OptionTransform {
  /** Transform an nullable value to Option. It returns Some(x) if the argument x is not null,
   *  and None if it is null.
   *
   *  @return   Some(value) if value != null, None if value == null
   */
  extension[T <: AnyRef](x: T | Null) def toOption: Option[T] =
    if x == null then None else Some(x)

  def test = {
    val x: String | Null = ???
    val y: Option[String] = x.toOption

    val xs: Array[String | Null] = ???
    val ys: Array[Option[String]] = xs.map(_.toOption)
  }
}