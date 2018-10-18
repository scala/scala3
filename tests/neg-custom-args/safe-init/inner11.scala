object NameKinds {
  abstract class NameInfo

  abstract class NameKind(val tag: Int) { self =>
    type ThisInfo <: Info

    @scala.annotation.init
    class Info extends NameInfo { this: ThisInfo =>
      def kind = self
    }
  }

  abstract class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info: Info = new Info
    info.kind // error
  }
}

object NameKinds {
  abstract class NameInfo

  abstract class NameKind(val tag: Int) { self =>
    type ThisInfo <: Info

    @scala.annotation.init
    class Info extends NameInfo { this: ThisInfo =>
      def kind = "info"
    }
  }

  abstract class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info: Info = new Info
    info.kind // ok
  }
}