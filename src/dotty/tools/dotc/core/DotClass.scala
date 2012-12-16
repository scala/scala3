package dotty.tools.dotc.core

class DotClass {

  def unsupported(methodName: String): Nothing =
    throw new UnsupportedOperationException(s"$getClass.$methodName")

}