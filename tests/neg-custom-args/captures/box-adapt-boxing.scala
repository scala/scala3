trait Cap

def main(io: Cap^, fs: Cap^): Unit = {
  val test1: Unit -> Unit = _ => {
    type Op = [T] -> (T ->{io} Unit) -> Unit
    val f: (Cap^{io}) -> Unit = ???
    val op: Op = ???
    op[Cap^{io}](f)   // error
    // expected type of f: {io} (box {io} Cap) -> Unit
    // actual type: ({io} Cap) -> Unit
    // adapting f to the expected type will also
    //   charge the environment with {io}
  }

  val test2: Unit -> Unit = _ => {
    type Box[X] = X
    type Op0[X] = Box[X] -> Unit
    type Op1[X] = Unit -> Box[X]
    val f: Unit -> (Cap^{io}) -> Unit = ???
    val test: Op1[Op0[Cap^{io}]^{io}]^{} = f
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
    val f: Unit -> (Cap^{io}) -> Unit = ???
    val g: Op[Id[Cap^{io}]^{fs}] = f
    val h: Op[Id[Cap^{io}]^{io}] = f
  }
}
