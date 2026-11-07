def test =
  val f1: [A] => WithLazy[A] = [A] => (x: A) => x // error
  val f2: [A] => WithObject[A] = [A] => (x: A) => x // error
  val f3: [A] => ViaPrivateLazyParent[A] = [A] => (x: A) => x // error
  val f4: [A] => ViaPrivateObjectParent[A] = [A] => (x: A) => x // error
