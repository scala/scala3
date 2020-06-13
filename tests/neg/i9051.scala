package zio:

  class ZRef
  object ZRef:

    private[zio] implicit class ZRefSyntax(private val self: ZRef):
      def unsafeUpdate: Boolean = true

object Main:
  val ref = new zio.ZRef
  println(ref.unsafeUpdate)  // error
