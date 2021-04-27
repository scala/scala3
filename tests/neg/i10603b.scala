import language.`3.0-migration`

object Test {
  val x = implicitly[List[Boolean]] // error
}
