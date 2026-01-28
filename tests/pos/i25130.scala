// Test case for issue #25130: Type inference failure for nested wildcard types

// Single-parameter type constructors
class S[T]
class M[T]
class N[T]

// Multi-parameter type constructors
class Pair[A, B]
class Triple[A, B, C]

object Test:
  // === Basic case: single nesting ===
  def foo[T](x: S[M[T]]): Unit = ()
  val x: S[M[?]] = new S
  foo(x)  // Wildcard type nested in generic container should match

  // This always worked (top-level wildcard):
  def bar[T](x: M[T]): Unit = ()
  val y: M[?] = null
  bar(y)

  // === Doubly-nested wildcards ===
  def doubleNested[T](x: S[M[N[T]]]): Unit = ()
  val dn: S[M[N[?]]] = new S
  doubleNested(dn)

  // === Multi-parameter types with wildcards ===

  // Wildcard in second position
  def pairSecond[T](x: Pair[String, M[T]]): Unit = ()
  val ps: Pair[String, M[?]] = new Pair
  pairSecond(ps)

  // Wildcard in first position
  def pairFirst[T](x: Pair[M[T], String]): Unit = ()
  val pf: Pair[M[?], String] = new Pair
  pairFirst(pf)

  // Wildcards in both positions (same type parameter)
  // Note: This requires the wildcards to unify, which only works
  // if they come from the same source. Two independent ? don't unify.
  def pairBothSame[T](x: Pair[M[T], N[T]]): Unit = ()
  // This wouldn't work: val pb: Pair[M[?], N[?]] = new Pair; pairBothSame(pb)
  // because the two ? are independent wildcards that can't unify to the same T

  // Different type parameters with wildcards
  def pairDifferent[A, B](x: Pair[M[A], N[B]]): Unit = ()
  val pd: Pair[M[?], N[?]] = new Pair
  pairDifferent(pd)

  // === Triple with various wildcard positions ===
  def tripleMiddle[T](x: Triple[String, M[T], Int]): Unit = ()
  val tm: Triple[String, M[?], Int] = new Triple
  tripleMiddle(tm)

  def tripleEnds[A, B](x: Triple[M[A], String, N[B]]): Unit = ()
  val te: Triple[M[?], String, N[?]] = new Triple
  tripleEnds(te)

  // === Nested in multi-parameter types ===
  def nestedInPair[T](x: Pair[S[M[T]], String]): Unit = ()
  val nip: Pair[S[M[?]], String] = new Pair
  nestedInPair(nip)

  // === Mixed nesting depths ===
  def mixedDepths[A, B](x: Pair[M[A], S[N[B]]]): Unit = ()
  val md: Pair[M[?], S[N[?]]] = new Pair
  mixedDepths(md)

  // === Using standard library types ===
  def withOption[T](x: Option[M[T]]): Unit = ()
  val wo: Option[M[?]] = None
  withOption(wo)

  def withList[T](x: List[M[T]]): Unit = ()
  val wl: List[M[?]] = Nil
  withList(wl)

  def withEither[A, B](x: Either[M[A], N[B]]): Unit = ()
  val we: Either[M[?], N[?]] = Left(new M)
  withEither(we)

  def withMap[K, V](x: Map[M[K], N[V]]): Unit = ()
  val wm: Map[M[?], N[?]] = Map.empty
  withMap(wm)

  // === Deeply nested with standard library ===
  def deepWithOption[T](x: Option[List[M[T]]]): Unit = ()
  val dwo: Option[List[M[?]]] = None
  deepWithOption(dwo)

  def tripleDeep[T](x: Option[List[Set[M[T]]]]): Unit = ()
  val td: Option[List[Set[M[?]]]] = None
  tripleDeep(td)
