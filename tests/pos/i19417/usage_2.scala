given[E](using e: EnumOf[E]): QueryParamDecoder[E] = QueryParamDecoder[String].emap(_ => Right(???))
trait EnumOf[E]