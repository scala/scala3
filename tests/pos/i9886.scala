class A(l: Any):
  def f = l

class B(l: => Any):
  def f = l

trait C(l: Any):
  def f = l

trait D(l: => Any):
  def f = l
