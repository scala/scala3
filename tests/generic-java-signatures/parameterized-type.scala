class ClassWithOption extends WithMap {
  val optionInClass: Option[String] = None
}
trait WithMap {
  val optionInTrait: Option[String] = None
}

object Test {
  def main(args: Array[String]) =
    classOf[ClassWithOption].getDeclaredFields.foreach { field =>
      println(s"${field.getName} ${field.getGenericType}")
    }
}
