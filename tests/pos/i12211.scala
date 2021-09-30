
def fst0[A, B[_]](a: A)(b: B[a.type]): a.type = a

def fst[A, B[_]]: (a: A) => (b: B[a.type]) => a.type =
  (a: A) => (b: B[a.type]) => a

def snd[A, B[_]]: (a: A) => () => (b: B[a.type]) => b.type =
  (a: A) => () => (b: B[a.type]) => b

def fst1[A, B[_]]: (a: A) => (b: B[a.type]) => a.type = fst0

def test1[A, B[_]]: (a: A) => () => (b: B[a.type]) => Any =
  snd[A, B]

def test2[A, B[_]]: (a: A) => (b: B[a.type]) => A = fst[A, B]

class AA
class BB[T]

def test3: (a: AA) => (b: BB[a.type]) => BB[?] =
  (a: AA) => (b: BB[a.type]) => b

trait RelaxedSelectable extends Selectable.WithoutPreciseParameterTypes:
  def applyDynamic(name: String, paramTypes: Class[_]*)(args: Any*): Any = ???

class Sink[A] extends RelaxedSelectable {
  def put(x: A): Unit = {}
}
val a = new Sink[String]
val b: RelaxedSelectable { def put(x: String): Unit } = a
val _ = b.put("")
