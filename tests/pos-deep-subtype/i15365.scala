trait Tagged[U]
type WithTag[+TT, UU] = TT & Tagged[UU]

trait FromInput[Val]
implicit def coercedScalaInput[Tc1]: FromInput[WithTag[Tc1, Int]] = ???
implicit def optionInput[To](implicit ev: FromInput[To]): FromInput[Option[To]] = ???

trait WithoutInputTypeTags[TW]
implicit def coercedOptArgTpe[Tc]: WithoutInputTypeTags[Option[WithTag[Tc, Int]]] = ???

trait InputType[+TI]
class OptionInputType[TO](ofType: InputType[TO]) extends InputType[Option[TO]]

type Argument[TA]
def argument[Ta](argumentType: InputType[Ta])(implicit fromInput: FromInput[Ta], res: WithoutInputTypeTags[Ta]): Argument[Option[Ta]] = ???

def test = argument(OptionInputType(??? : InputType[WithTag[Boolean, Int]])) :: Nil // error
