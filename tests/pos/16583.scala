import scala.compiletime.constValueTuple

val ll0: Tuple3["one", "two", "three"] = constValueTuple[("one", "two", "three")]
val ll1 = constValueTuple[("one", "two", "three")].toList
val ll3: List["one" | ("two" | ("three" | Nothing))] = constValueTuple[("one", "two", "three")].toList
val ll4: List["one" | ("two" | "three")] = constValueTuple[("one", "two", "three")].toList

inline def labels[Labels <: Tuple](using ev: Tuple.Union[Labels] <:< String): List[String] =
  val tmp = constValueTuple[Labels].toList
  ev.substituteCo(tmp)

def test = labels[("one", "two", "three")]

def toList(x: Tuple): List[Tuple.Union[x.type]] = ???
def test2[Labels <: Tuple] = toList((???): Labels)

def i16654 =
  def t1: Tuple = EmptyTuple
  val t2 = t1.toList
