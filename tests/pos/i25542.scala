class PartialRequest:
  protected def handle(i: Int): Int = i

object PartialRequest:
  trait Bundler:
    def bundle(req: PartialRequest): Int => Int

  object Bundler:
    inline given Bundler = new Bundler:
      def bundle(req: PartialRequest): Int => Int =
        (i: Int) => req.handle(i)

  def apply()(using bundler: Bundler): Int => Int =
    bundler.bundle(new PartialRequest)

val request = PartialRequest()
