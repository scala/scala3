class Wrapper[E <: Throwable]:
  object syntax:
    extension [A <: Matchable](a: E | A)
      transparent inline def foreach(f: A => Any): Unit =
        a match
          case e: E => ()
          case a: A => f(a)

  def _catch[A](a: => A): E | A =
    try a
    catch case e: E => e

object throwables extends Wrapper[Throwable]
import throwables.syntax.*

@main def Test(): Unit =
  for x <- throwables._catch(1/1) do println(x)
