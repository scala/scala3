object Example extends App {

  final case class Foo[A](run: A ?=> Int)

}