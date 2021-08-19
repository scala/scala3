case class Field(name: String, subQuery: Option[Query] = None)
case class Query(fields: Seq[Field])
val x = Query(Seq(Field("a", subQuery=Some(Query(Seq(Field("b")), Nil)))), Nil) // error
