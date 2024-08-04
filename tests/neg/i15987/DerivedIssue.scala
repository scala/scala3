import scala.language.experimental.clauseInterleaving

trait ShowWithExplicit[A]

object ShowWithExplicit:
  def derived[A, B](explicit: String)(using DummyImplicit)(implicit dummy: DummyImplicit): ShowWithExplicit[A] = ???

trait ShowUsingAndImplicit[A]

object ShowUsingAndImplicit:
  def derived[A, B](using DummyImplicit)(implicit dummy: DummyImplicit): ShowUsingAndImplicit[A] = ???

trait ShowUsing[A]

object ShowUsing:
  def derived[A](using DummyImplicit): ShowUsing[A] = ???

trait ShowImplicit[A]

object ShowImplicit:
  def derived[A](implicit ev: DummyImplicit): ShowImplicit[A] = ???

trait ShowContra[-A]

object ShowContra:
  val derived: ShowContra[Any] = ???

case class Person(name: String) derives ShowWithExplicit, // error
  ShowUsingAndImplicit,
  ShowUsing,
  ShowImplicit,
  ShowContra
