abstract class Foo[B] extends Bar[B]

trait Bar[A]:
  self: Foo[A] =>

  def foo       : Unit   = bar(???)
  def bar(f: A) : Unit

