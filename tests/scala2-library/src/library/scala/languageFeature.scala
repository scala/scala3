/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2015, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala
import scala.annotation.meta.{languageFeature => feature}

object languageFeature {

  @feature("extension of type scala.Dynamic", enableRequired = true)
  sealed trait dynamics
  object dynamics extends dynamics

  @feature("postfix operator #", enableRequired = false)
  sealed trait postfixOps
  object postfixOps extends postfixOps

  @feature("reflective access of structural type member #", enableRequired = false)
  sealed trait reflectiveCalls
  object reflectiveCalls extends reflectiveCalls

  @feature("implicit conversion #", enableRequired = false)
  sealed trait implicitConversions
  object implicitConversions extends implicitConversions

  @feature("higher-kinded type", enableRequired = false)
  sealed trait higherKinds
  object higherKinds extends higherKinds

  @feature("#, which cannot be expressed by wildcards, ", enableRequired = false)
  sealed trait existentials
  object existentials extends existentials

  object experimental {
    @feature("macro definition", enableRequired = true)
    sealed trait macros
    object macros extends macros
  }
}

