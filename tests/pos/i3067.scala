class Test[T](f: List[String] => T)

object o {
  implicit object a extends Test[List[String]](_ map identity)
  implicit object b extends Test[List[String]](_ map identity)
}
