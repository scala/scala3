final class Baz private (val x: Int) extends AnyVal {
  def `unary_!_=`() : Baz = ??? // parses ok, but will not be usable
  def `unary_~_=`() : Baz = ??? // parses ok, but will not be usable
  def `unary_+_=`() : Baz = ??? // parses ok, but will not be usable
  def `unary_-_=`() : Baz = ??? // parses ok, but will not be usable
}
