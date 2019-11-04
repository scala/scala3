// Test the handling of generics by the nullability transform.
// There are two classes here: JavaCat is Java-defined, and ScalaCat
// is Scala-defined.

class ScalaCat[T] {}

class Test {
  // It's safe to return a JavaCat[String]|Null (no inner |Null),
  // because JavaCat, being a Java class, _already_ nullifies its
  // fields.
  val jc: JavaCat[String]|Null = J.getJavaCat[String]()
  // ScalaCat is Scala-defined, so we need the inner |Null.
  val sc: ScalaCat[String|Null]|Null = J.getScalaCat[String]()

  import java.util.List

  val las: List[Array[String|Null]]|Null = J.getListOfStringArray()
  val als: Array[List[String]|Null]|Null = J.getArrayOfStringList()
  val css: List[Array[List[Array[String|Null]]|Null]]|Null = J.getComplexStrings()
}
