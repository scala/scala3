package test.syntaxes

class DoingStuffOps[A](a: A):
  def doStuff: Unit = ()

trait DoingStuffSyntax:
  implicit def toDoingStuffOps[A](a: A): DoingStuffOps[A] = DoingStuffOps(a)

object doingstuff extends DoingStuffSyntax
