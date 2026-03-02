// https://github.com/scala/scala3/issues/7653

object options2 {
  type Option[T] = {
    def isEmpty: Boolean
  }
  type None[T] = Option[T]
  val none: () => Option[Nothing] = () =>
    new {
      def isEmpty = true
    }
  val mkNone0: [T] => () => Option[Nothing] = [T] =>
    () =>
      new {
        def isEmpty = true
    }
  val mkNone: [T] => () => Option[T] = [T] =>
    () =>
      new {
        def isEmpty = true
    }
}
