package foo

class C(s: String):
  def f = s.trim // warn
  def g: String  =
    val s2 = s.trim
    s2
  def h = (s.trim, s.length) // warn
  protected def i = s.trim // warn
  private def j = s.trim
  private[foo] def k = s.trim // warn
  val ss = s.replace("a", "A") // warn
  val ss2 = Seq(s.trim) // warn
  val ss3: Seq[String] =
    val s3 = s.trim
    Seq(s3)

val s2: String = ""

object O:
  def f = s2.trim // warn
  def g: String  =
    val s3 = s2.trim
    s3
  def h = (s2.trim, s2.length) // warn
  val ss = s2.replace("a", "A") // warn
  val ss2 = Seq(s2.trim) // warn
  val ss3: Seq[String] =
    val s3 = s2.trim
    Seq(s3)

def f = s2.trim // warn
def g: String  =
  val s3 = s2.trim
  s3
def h = (s2.trim, s2.length) // warn
val ss = s2.replace("a", "A") // warn
val ss2 = Seq(s2.trim) // warn
val ss3: Seq[String] =
  val s3 = s2.trim
  Seq(s3)