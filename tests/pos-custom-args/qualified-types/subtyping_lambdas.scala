def toBool[T](x: T): Boolean = ???
def tp[T](): Any = ???


def test: Unit =
  val v1: {l: List[Int] with l.forall(x => x > 0)} = ??? : {l: List[Int] with l.forall(x => x > 0)}
  val v2: {l: List[Int] with l.forall(x => x > 0)} = ??? : {l: List[Int] with l.forall(y => y > 0)}
  val v3: {l: List[Int] with l.forall(x => x > 0)} = ??? : {l: List[Int] with l.forall(_ > 0)}

  val v4: {l: List[Int] with toBool((x: String) => x.length > 0)} = ??? : {l: List[Int] with toBool((y: String) => y.length > 0)}

  val v5: {l: List[Int] with toBool((x: String) => tp[x.type]())} = ??? : {l: List[Int] with toBool((y: String) => tp[y.type]())}
  val v6: {l: List[Int] with toBool((x: String, y: String) => tp[x.type]())} = ??? : {l: List[Int] with toBool((x: String, y: String) => tp[x.type]())}
  val v7: {l: List[Int] with toBool((x: String) => tp[x.type]())} = ??? : {l: List[Int] with toBool((y: String) => tp[y.type]())}
  val v8: {l: List[Int] with toBool((x: String, y: String) => tp[y.type]())} = ??? : {l: List[Int] with toBool((x: String, y: String) => tp[y.type]())}

  val v9: {l: List[Int] with toBool((x: String) => (y: String) => x == y)} = ??? : {l: List[Int] with toBool((a: String) => (b: String) => a == b)}
