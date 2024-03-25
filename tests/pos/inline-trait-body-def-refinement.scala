// FIXME: scalac tests/pos/inline-trait-body-def-refinement.scala -Yforce-sbt-phases -Xprint:typer

trait F:
  def a: A

inline trait A:
  def f: F { def a: A } = ???

class B extends A
