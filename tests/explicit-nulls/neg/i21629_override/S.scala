import javax.annotation.J

class S extends J {
  // @Nullable argument: Java param is String | Null, override must accept String | Null
  override def argNullable(s: String): String = ??? // error

  // @Nullable type parameter: Java returns List[String | Null], override must match
  override def returnListOfNullable(): java.util.List[String] = ??? // error
}

class SOk extends J {
  override def argNullable(s: String | Null): String = s match
    case null => "null"
    case s => s

  // Narrowing return type is valid (covariant): String <: String | Null
  override def returnNullable(): String = "ok"

  override def returnListOfNullable(): java.util.List[String | Null] = java.util.ArrayList()
}
