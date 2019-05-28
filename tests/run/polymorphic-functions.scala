object Test extends App {
  // Types
  type F0 = [T] => List[T] => Option[T]
  type F1 = [F[_], G[_], T] => (F[T], F[T] => G[T]) => G[T]

  // Terms
  val t0 = [T] => (ts: List[T]) => ts.headOption
  val t0a: F0 = t0
  assert(t0(List(1, 2, 3)) == Some(1))

  val t1 = [F[_], G[_], T] => (ft: F[T], f: F[T] => G[T]) => f(ft)
  val t1a: F1 = t1
  assert(t1(List(1, 2, 3), (ts: List[Int]) => ts.headOption) == Some(1))
}
