



object wrap {

  trait DomainLike[@specialized(Int) A, +Self <: Domain[A]]

  trait Domain[@specialized(Int) B]
  extends DomainLike[B, Domain[B]]

  trait IterableDomainLike[@specialized(Int) C, +Self <: IterableDomain[C]]
  extends DomainLike[C, Self]

  trait IterableDomain[@specialized(Int) D]
  extends Domain[D] with IterableDomainLike[D, IterableDomain[D]]

}
