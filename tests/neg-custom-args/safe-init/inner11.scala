object NameKinds {
  val a = b // error

  abstract class NameInfo

  abstract class NameKind(val tag: Int) { self =>
    type ThisInfo <: Info

    /** A simple info type; some subclasses of Kind define more refined versions */
    @scala.annotation.partial
    class Info extends NameInfo { this: ThisInfo =>
      def kind = self
    }
  }

  abstract class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info: Info @scala.annotation.filled = new Info
  }

  val b = 0
}