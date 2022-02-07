import language.`3.0-migration`

def given = 42 // error

case class C(enum: List[Int] = Nil) { // error
  val s = s"$enum" // error
}
