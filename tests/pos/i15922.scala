trait Cap:
  type M
class Id[X]

object Test:
  def withCap[X](op: Cap => X): X = ???

  class retains1(xs: Any*) extends annotation.StaticAnnotation

  def leaking1(c: Cap): Id[Cap @retains1(c)] = ??? // used to crash with orphan parameter on pickling
  def leaking2(c: Cap): Id[c.type] = ???

  val bad1 = withCap(leaking1)
  val bad2 = withCap(leaking2)
