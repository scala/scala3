import scala.reflect.Typeable

def unionTypeTest[T: Typeable](m: Int|T) =
  m match
  case x: Int => println("Got Int")
  case t: T => println("Got T")

@main def run =
  unionTypeTest(())
