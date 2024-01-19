trait QueryParamDecoder[E]:
  def emap[T](fn: E => Either[Throwable, T]): QueryParamDecoder[T]
object QueryParamDecoder:
  def apply[T](implicit ev: QueryParamDecoder[T]): QueryParamDecoder[T] = ev
  implicit lazy val stringQueryParamDecoder: QueryParamDecoder[String] = ???