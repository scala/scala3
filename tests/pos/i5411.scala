trait A
trait B
object O {
  def m(x: Seq[A & B]) = java.util.Arrays.asList(x*)
}