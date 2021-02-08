// compile with -Ycheck:reifyQuotes -Ystop-after:reifyQuotes
import quoted.*

sealed abstract class SomeEnum
object SomeEnum:
  final val Foo = new SomeEnum {}

def quoteFoo: Quotes ?=> Expr[SomeEnum.Foo.type] = '{SomeEnum.Foo}
