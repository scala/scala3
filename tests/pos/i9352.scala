abstract class Foo[B] extends Bar[B]

trait Bar[A] with
  self: Foo[A] =>

  def foo       : Unit   = bar(???)
  def bar(f: A) : Unit

