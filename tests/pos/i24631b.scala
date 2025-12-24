//> using options -source:3.0-migration

abstract class C[A]:
  def create(s: String): A
  def create(): A = create("")

class D extends C[String]:
  def create(s: String): String = s

val s = D().create
val s2: String = s
