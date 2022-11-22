class X:
  def toString(maxLines: Int = 10, maxWidth: Int = 10): String = (maxLines -> maxWidth).toString

class Foo extends X:
  override def toString(maxLines: Int, maxWidth: Int): String = s"overriden ($maxLines, $maxWidth)"
  override def toString(): String = toString(maxLines = 3)


@main def Test = {
  println(Foo().toString())
}