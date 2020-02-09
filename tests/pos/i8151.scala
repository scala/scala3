trait A
trait B
object O{
  def m(using x:A = null)(using y:B = null) = 1
  def n = m
}