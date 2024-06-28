

import language.`3.0-migration`

def given = 42 // warn

case class C(enum: List[Int] = Nil) { // warn
  val s = s"$enum" // warn
}