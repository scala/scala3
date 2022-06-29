
trait WithPath:
  type A

trait Proto:
  type A <: WithPath
  def func(a: A): a.A

object Impl extends Proto:
  type A = Nothing
  def func(a: A) = a // error