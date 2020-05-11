// compile with -Ycheck:reifyQuotes -Ystop-after:reifyQuotes
import quoted._

sealed abstract class SomeEnum
object SomeEnum:
  final val Foo = new SomeEnum {}

def quoteFoo: (s: Scope) ?=> s.Expr[SomeEnum.Foo.type] = '{SomeEnum.Foo}
