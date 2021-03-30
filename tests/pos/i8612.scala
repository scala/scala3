object Foo1:
  def assert1(x: Boolean) = if !x then ???
  inline def assert2(x: Boolean) = if !x then ???
  inline def assert3(inline x: Boolean) = if !x then ???

  assert1(???)
  assert2(???)
  assert3(???)

object Foo2:
  def assert1(x: Boolean) = if !x then ???
  transparent inline def assert2(x: Boolean) = if !x then ???
  transparent inline def assert3(inline x: Boolean) = if !x then ???

  assert1(???)
  assert2(???)
  assert3(???)
