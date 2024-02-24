case class State(x: Int)

def foo[T](state: State)(body: => T): Option[T] // error: only classes can have declared but undefined members
  Option.when(state.x == 0) body // error: Illegal start of toplevel definition

var bar = 0
val state = State(0)

def app: Function1[Int, Unit] =
  new Function1[Int, Unit]:
    def apply(x: Int): Unit =
      foo(state):
        foo(state.copy(x = 5): // Missing ")" // error: method copy in class State does not take more parameters
          println("a")
      bar = 2 // error: ',' or ')' expected
