trait Cmp[A, B]
object Cmp:
  given sub[A, B](using A <:< B): Cmp[A, B] = new Cmp[A, B] {}

def eq[A, B](a: A, b: B)(using Cmp[A, B]): Unit = ()

val x: Int | String = 1
val y: Int | String = ""

def needs(using ev: (x.type | y.type) <:< (x.type | y.type)) = ev
def direct = needs
def summoned = summon[Cmp[x.type | y.type, x.type | y.type]]
def explicitArgs = eq[x.type | y.type, x.type | y.type](x, y)
