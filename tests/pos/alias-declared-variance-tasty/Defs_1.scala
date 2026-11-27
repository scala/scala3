package aliasvariance

import scala.annotation.unchecked.uncheckedVariance

trait HasAbstract:
  // abstract type member: body is a TypeBounds, so `fromParams` already used the
  // declared variances
  type F[-X] <: List[X @uncheckedVariance]

// class type parameter: variance is a symbol flag
class Klass[-T]

object Defs:
  // opaque alias: re-abstracted through `opaqueToBounds`
  opaque type Op[-T] = List[T @uncheckedVariance]

  // transparent alias: the declared variance used to be dropped when read from TASTy
  type Tr[-T] = List[T @uncheckedVariance]

  // several parameters, only some of them annotated
  type Fn[A, +B] = A => B
