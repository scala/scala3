trait A
trait B
object O{
  def m(using x:A = ???)(using y:B = ???) = 1
  def n = m
}