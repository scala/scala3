def toBool[T](x: T): Boolean = ???
def tp[T](): Any = ???

def test: Unit =
  val x: {l: List[Int] with toBool((x: String, y: x.type) => x.length > 0)} = ??? // error: cannot turn method type into closure because it has internal parameter dependencies
  summon[{l: List[Int] with toBool((x: String, y: String) => tp[x.type]())} =:= {l: List[Int] with toBool((x: String, y: String) => tp[y.type]())}] // error

  summon[{l: List[Int] with toBool((x: Double) => (y: Int) => x == y)} =:= {l: List[Int] with toBool((a: Double) => (b: Int) => a == a)}] // error
  summon[{l: List[Int] with toBool((x: Int) => (y: Int) => x == y)} =:= {l: List[Int] with toBool((a: Int) => (b: Int) => a == a)}] // error
