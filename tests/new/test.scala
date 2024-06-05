import language.experimental.namedTuples

type Person = (name: String, age: Int)

trait A:
  type T

class B:
  type U =:= A { type T = U }

