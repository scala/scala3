object Example extends App {
  final case class Foo[A](run: A ?=> Int) {
    // def copy[A]: this.A ?=> Int = (using a) => run
  }
}
