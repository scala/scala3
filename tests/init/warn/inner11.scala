object NameKinds {
  abstract class NameInfo

  abstract class NameKind(val tag: Int) { self =>
    type ThisInfo <: Info

    class Info extends NameInfo { this: ThisInfo =>
      def kind = self
    }
  }

  class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info: Info = new Info
    println(info.kind)                     // warn
    val n = 10
  }
}

object NameKinds2 {
  abstract class NameInfo

  abstract class NameKind(val tag: Int) { self =>
    type ThisInfo <: Info

    class Info extends NameInfo { this: ThisInfo =>
      def kind = "info"
    }
  }

  class ClassifiedNameKind(tag: Int, val infoString: String) extends NameKind(tag) {
    type ThisInfo = Info
    val info: Info = new Info
    println(info.kind) // ok
    val count: Int = 10
  }
}
