def toBool[T](x: T): Boolean = ???
def tp[T](): Any = ???


def test: Unit =
  summon[{l: List[Int] with l.forall(x => x > 0)} =:= {l: List[Int] with l.forall(x => x > 0)}]
  summon[{l: List[Int] with l.forall(x => x > 0)} =:= {l: List[Int] with l.forall(y => y > 0)}]
  summon[{l: List[Int] with l.forall(x => x > 0)} =:= {l: List[Int] with l.forall(_ > 0)}]

  summon[{l: List[Int] with toBool((x: String) => x.length > 0)} =:= {l: List[Int] with toBool((y: String) => y.length > 0)}]

  summon[{l: List[Int] with toBool((x: String) => tp[x.type]())} =:= {l: List[Int] with toBool((y: String) => tp[y.type]())}]
  summon[{l: List[Int] with toBool((x: String, y: String) => tp[x.type]())} =:= {l: List[Int] with toBool((x: String, y: String) => tp[x.type]())}]
  summon[{l: List[Int] with toBool((x: String) => tp[x.type]())} =:= {l: List[Int] with toBool((y: String) => tp[y.type]())}]
  summon[{l: List[Int] with toBool((x: String, y: String) => tp[y.type]())} =:= {l: List[Int] with toBool((x: String, y: String) => tp[y.type]())}]

  summon[{l: List[Int] with toBool((x: String) => (y: String) => x == y)} =:= {l: List[Int] with toBool((a: String) => (b: String) => a == b)}]
