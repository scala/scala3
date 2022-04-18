trait A {
  def combine(another: A): A = ???
}

trait Dsl {
  def example: A
}

object Dsl {
  def execute(program: (dsl: Dsl) ?=> Int): String = ???
}

def example(using a: Dsl): A = ???

import Dsl._

def demo = Dsl.execute {
  // change to `combine` and it compiles
  example notCombine example  // error
  42
}