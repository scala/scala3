package example

trait Example {
  class Input

  type Output[A] = A match {
    case Input => Int
  }
}

class Ref(ref: Example#Input)
