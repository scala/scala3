package test.syntaxes

class DoingStuffOps[A](a: A):
  def doStuff: Unit = ()

trait DoingStuffSyntax:
  implicit def toDoingStuffOps[A](a: A): DoingStuffOps[A] = DoingStuffOps(a)

trait AllSyntaxes extends DoingStuffSyntax

object doingstuff extends AllSyntaxes
