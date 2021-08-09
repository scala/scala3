trait Vehicle
trait Car extends Vehicle

trait Encoder[A]
object Encoder {
  implicit val encodeVehicle: Encoder[Vehicle] = ???
  implicit val encodeCar: Encoder[Car] = ???
}

trait Route
trait Printer
trait Marshaller[-A]  // must be contravariant

object Test {
  implicit def marshaller[A: Encoder](implicit p: Printer = ???): Marshaller[A] = ???
    // the `Printer` implicit arg seems to be necessary, either with default value, or no implicit in scope

  def foo[A](v: A)(implicit m: Marshaller[A]): Route = ???

  val route: Route = identity {
    val f: (Car => Route) => Route = ???  // ok if s/Car/Vehicle/
    f(vehicle => foo(vehicle)) // error: ambiguous implicit
  }
}
