trait Tagged[U]
type WithTag[+T, U] = T & Tagged[U]

trait FromInput[Val]
implicit def coercedScalaInput[T]: FromInput[T & Tagged[Int]] = ???
implicit def optionInput[T](implicit ev: FromInput[T]): FromInput[Option[T]] = ???

trait WithoutInputTypeTags[T]
implicit def coercedOptArgTpe[T]: WithoutInputTypeTags[Option[T & Tagged[Int]]] = ???

trait InputType[+T]
class OptionInputType[T](ofType: InputType[T]) extends InputType[Option[T]]

type Argument[T]
def argument[T](argumentType: InputType[T])(implicit fromInput: FromInput[T], res: WithoutInputTypeTags[T]): Argument[Option[T]] = ???

def test = argument(OptionInputType(??? : InputType[Boolean & Tagged[Int]]))
