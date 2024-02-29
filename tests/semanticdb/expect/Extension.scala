package ext

extension (s: String)
  def foo: Int = 42
  def #*# (i: Int): (String, Int) = (s, i)

val a = "asd".foo

val c = "foo" #*# 23

trait Read[+T]:
  def fromString(s: String): Option[T]

extension (s: String)
  def readInto[T](using Read[T]): Option[T] = summon[Read[T]].fromString(s)

trait Functor[F[_]]:
  extension [T](t: F[T]) def map[U](f: T => U): F[U]

opaque type Deck = Long
object Deck:
  extension (data: Deck)
    def fooSize: Int = ???

object DeckUsage:
  val deck: Deck = ???
  deck.fooSize
