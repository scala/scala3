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
  extension [A, E](x: Maybe[A, E])
    transparent inline def ? (using maybe.CanErr[E]): A =
      x.runtimeChecked match  // runtime checked needed for non-bootstrapped compilation
      case Ok(y) => y
      case Err(e) => break(Err(e))

    def withErr[E1](e: E1): Maybe[A, E1] = x.runtimeChecked match
      case Ok(y) => Ok(y)
      case Err(_) => Err(e)

    def mapErr[E1](f: E => E1): Maybe[A, E1] = x.runtimeChecked match
      case Ok(y) => Ok(y)
      case Err(e) => Err(f(e))

    def map[B](f: A => B): Maybe[B, E] = x.runtimeChecked match
      case Ok(y) => Ok(f(y))
      case Err(e) => Err(e)

    def flatMap[B](f: A => Maybe[B, E]): Maybe[B, E] = x.runtimeChecked match
      case Ok(y) => f(y)
      case Err(e) => Err(e)

    def foreach[U](f: A => U): Unit = x.runtimeChecked match
      case Ok(y) => f(y)
      case Err(_) =>

  extension [A](x: Maybe[A, Unit])
    def filter(p: A => Boolean): Maybe[A, Unit] = x.runtimeChecked match
      case Ok(y) if p(y) => x
      case _ => null.asInstanceOf[Maybe[A, Unit]]

    def filterNot(p: A => Boolean): Maybe[A, Unit] = x.filter(!p(_))

    inline def withFilter(p: A => Boolean): WithFilter[A] = WithFilter(x, p)

  class WithFilter[A](x: Maybe[A, Unit], p: A => Boolean):
    def map[B](f: A => B): Maybe[B, Unit] = x.filter(p).map(f)
    def flatMap[B](f: A => Maybe[B, Unit]): Maybe[B, Unit] = x.filter(p).flatMap(f)
    def foreach[U](f: A => U): Unit = x.filter(p).foreach(f)
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter(x, x => p(x) && q(x))
}


