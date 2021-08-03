// original report
def foo(opt: Option[Tuple.Tail[NonEmptyTuple]]): Unit =
  opt match
    case None => ???
    case Some(a) => ???


// again with a mini-Tuple with the extra NonEmptyTupExtra parent, to test transitivity
object WithExtraParent:
  sealed trait Tup

  object Tup {
    type Tail[X <: NonEmptyTup] <: Tup = X match {
      case _ **: xs => xs
    }
  }

  object EmptyTup extends Tup

  sealed trait NonEmptyTup extends Tup
  sealed trait NonEmptyTupExtra extends NonEmptyTup
  sealed abstract class **:[+H, +T <: Tup] extends NonEmptyTupExtra

  object **: {
    def unapply[H, T <: Tup](x: H **: T): (H, T) = null
  }

  def foo(opt: Option[Tup.Tail[NonEmptyTup]]): Unit =
    opt match
      case None => ???
      case Some(a) => ???
end WithExtraParent


// again with a non-abstract parent
object WithNonAbstractParent:
  sealed trait Tup

  object Tup {
    type Tail[X <: NonEmptyTup] <: Tup = X match {
      case _ **: xs => xs
    }
  }

  object EmptyTup extends Tup

  sealed class NonEmptyTup extends Tup
  sealed class **:[+H, +T <: Tup] extends NonEmptyTup

  object **: {
    def unapply[H, T <: Tup](x: H **: T): (H, T) = null
  }

  def foo(opt: Option[Tup.Tail[NonEmptyTup]]): Unit =
    opt match
      case None => ???
      case Some(a) => ???
end WithNonAbstractParent


// again with multiple children, but an exhaustive match
object WithExhaustiveMatch:
  sealed trait Tup

  object Tup {
    type Tail[X <: NonEmptyTup] <: Tup = X match {
      case _ **: xs => xs
      case _ *+: xs => xs
    }
  }

  object EmptyTup extends Tup

  sealed trait NonEmptyTup extends Tup
  sealed abstract class **:[+H, +T <: Tup] extends NonEmptyTup
  sealed abstract class *+:[+H, +T <: Tup] extends NonEmptyTup

  object **: {
    def unapply[H, T <: Tup](x: H **: T): (H, T) = null
  }
  object *+: {
    def unapply[H, T <: Tup](x: H *+: T): (H, T) = null
  }

  def foo(opt: Option[Tup.Tail[NonEmptyTup]]): Unit =
    opt match
      case None => ???
      case Some(a) => ???
end WithExhaustiveMatch
