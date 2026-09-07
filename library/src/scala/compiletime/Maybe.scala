package scala.compiletime

import scala.runtime.Valid
import annotation.experimental
import scala.util.boundary.break
import scala.util.{Ok, Err}

/** Under experimental.maybe, a trait backing maybe types `T?` */
@experimental
sealed trait Maybe[+T, +E] extends Any, Matchable:
  def isEmpty: Boolean
  def get: T

object Maybe {
  extension [T, E](x: Maybe[T, E])
    transparent inline def ? (using maybe.CanErr[E]): T =
      x.runtimeChecked match  // runtime checked needed for non-bootstrapped compilation
      case Ok(y) => y
      case Err(e) => break(Err(e))

    def withErr[E1](e: E1): Maybe[T, E1] = x.runtimeChecked match
      case Ok(y) => Ok(y)
      case Err(_) => Err(e)

    def mapErr[E1](f: E => E1): Maybe[T, E1] = x.runtimeChecked match
      case Ok(y) => Ok(y)
      case Err(e) => Err(f(e))

    def map[U](f: T => U): Maybe[U, E] = x.runtimeChecked match
      case Ok(y) => Ok(f(y))
      case Err(e) => Err(e)

    def flatMap[U](f: T => Maybe[U, E]): Maybe[U, E] = x.runtimeChecked match
      case Ok(y) => f(y)
      case Err(e) => Err(e)

    // TODO Add withFilter


}


