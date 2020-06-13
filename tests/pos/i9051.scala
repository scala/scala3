package zio

class ZRef

object ZRef {

  private[zio] implicit class ZRefSyntax(private val self: ZRef) extends AnyVal {
    def unsafeUpdate: Boolean = true
  }
}

object Main extends App {
  val ref = new ZRef
  println(ref.unsafeUpdate)
}