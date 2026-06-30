/**
  * Contains a convenient default pre-configured PPrinter.
  *
  * Hard-coded and inflexible, but feel free to instantiate your own
  * PPrint if you want to customize it.
  */
package object pprint extends PPrinter{
  def tprint[T: TPrint](implicit config: TPrintColors) = {
    implicitly[TPrint[T]].render
  }

  implicit class BlackWhiteContext(private val self: StringContext) extends AnyVal {
    def bw(args: Any*): String = PPrinter.BlackWhite.interpolate(self, args*)
  }
  implicit class ColorContext(private val self: StringContext) extends AnyVal {
    def pp(args: Any*): String = PPrinter.Color.interpolate(self, args*)
  }
}
