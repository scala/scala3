trait Box {
  type T
}

object Test {
  def main(args: Array[String]): Unit = {
    assert(!isFunctionType[Option[Int]])
    assert(!isFunctionType[String])

    assert(!isFunctionType[List[Int]])
    assert(!isFunctionType[Set[Int]])

    // TODO: compiler failed to synthesize Type[T]
    // type T = given Set[Int] => Int
    // assert(isFunctionType[T])

    // TODO: compiler failed to synthesize Type[T]
    // type T = Int => Int
    // assert(isFunctionType[T])

    assert(isFunctionType[(b: Box) => b.T])
    assert(isFunctionType[() => Int])
    assert(isFunctionType[(Int) => Int])
    assert(isFunctionType[(Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])
    assert(isFunctionType[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int])

    assert(isDependentFunctionType[(b: Box) => b.T])
    assert(!isDependentFunctionType[Int => Int])
    // type A = (b: Box) => b.T
    // assert(isDependentFunctionType[A])

    assert(isImplicitFunctionType[given Int => Int])
    assert(!isImplicitFunctionType[Int => Int])
    // type B = given Set[Int] => Int
    // assert(isImplicitFunctionType[B])

    assert(isErasedFunctionType[erased Int => Int])
    assert(!isErasedFunctionType[Int => Int])
    // type C = erased Set[Int] => Int
    // assert(isErasedFunctionType[C])
  }
}
