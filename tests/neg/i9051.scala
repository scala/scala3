package zio:

  class ZRef
  object ZRef with

    private[zio] implicit class ZRefSyntax(private val self: ZRef) with
      def unsafeUpdate: Boolean = true

object Main with
  val ref = new zio.ZRef
  println(ref.unsafeUpdate)  // error
