trait Cap

def main(io: {*} Cap, fs: {*} Cap): Unit = {
  val test1: {} Unit -> Unit = _ => { // error
    type Op = [T] -> ({io} T -> Unit) -> Unit
    val f: ({io} Cap) -> Unit = ???
    val op: Op = ???
    op[{io} Cap](f)
    // expected type of f: {io} (box {io} Cap) -> Unit
    // actual type: ({io} Cap) -> Unit
    // adapting f to the expected type will also
    //   charge the environment with {io}
  }

  val test2: {} Unit -> Unit = _ => {
    type Box[X] = X
    type Op0[X] = Box[X] -> Unit
    type Op1[X] = Unit -> Box[X]
    val f: Unit -> ({io} Cap) -> Unit = ???
    val test: {} Op1[{io} Op0[{io} Cap]] = f
    // expected: {} Unit -> box {io} (box {io} Cap) -> Unit
    // actual: Unit -> ({io} Cap) -> Unit
    //
    // although adapting `({io} Cap) -> Unit` to
    // `box {io} (box {io} Cap) -> Unit` will leak the
    // captured variables {io}, but since it is inside a box,
    // we will charge neither the outer type nor the environment
  }

  val test3 = {
    type Box[X] = X
    type Id[X] = Box[X] -> Unit
    type Op[X] = Unit -> Box[X]
    val f: Unit -> ({io} Cap) -> Unit = ???
    val g: Op[{fs} Id[{io} Cap]] = f // error
    val h: {} Op[{io} Id[{io} Cap]] = f
  }
}
