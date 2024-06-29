//> using options -Wunused:imports

trait Schema[A]

case class Foo()
case class Bar()

trait SchemaGenerator[A] {
  given Schema[A] = new Schema[A]{}
}

object FooCodec extends SchemaGenerator[Foo]
object BarCodec extends SchemaGenerator[Bar]

def summonSchemas(using Schema[Foo], Schema[Bar]) = ()

def summonSchema(using Schema[Foo]) = ()

def `i19657 check prefix to pick selector`: Unit =
  import FooCodec.given
  import BarCodec.given
  summonSchemas

def `i19657 regression test`: Unit =
  import FooCodec.given
  import BarCodec.given // warn
  summonSchema

def `i19657 check prefix to pick specific selector`: Unit =
  import FooCodec.given_Schema_A
  import BarCodec.given_Schema_A
  summonSchemas

def `same symbol different names`: Unit =
  import FooCodec.given_Schema_A
  import FooCodec.given_Schema_A as AThing
  summonSchema(using given_Schema_A)
  summonSchema(using AThing)

