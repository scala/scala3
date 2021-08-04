object Wrapper:
  type E <: Throwable
  class C:
    object syntax:
        extension [A <: Matchable](a: E | A)
          transparent inline def foreach(f: A => Any): Unit =
            a match
              case e: E =>
                Wrapper.this.n + m
              case a: A => f(a)
        val m: Int = 4

  def _catch[A](a: => A): E | A =
    try a
    catch case e: E => e

  val n: Int = 3

object throwables extends Wrapper.C
import throwables.syntax.*

@main def Test(): Unit =
  for x <- Wrapper._catch(1/1) do println(x)