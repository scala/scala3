import scala.quoted.*

transparent inline def thing =
  ${ thingImpl }

def thingImpl(using Quotes): Expr[Any] =
  '{
    def makeThing: { def me: this.type } = ???
    makeThing
  }
