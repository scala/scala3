import language.experimental.erasedDefinitions

class CanSerialize[T]

inline given CanSerialize[String] = CanSerialize()
inline given [T: CanSerialize] => CanSerialize[List[T]] = CanSerialize()

def safeWriteObject[T <: java.io.Serializable](out: java.io.ObjectOutputStream, x: T)(using erased CanSerialize[T]) =
  out.writeObject(x)

def writeList[T](out: java.io.ObjectOutputStream, xs: List[T])(using erased CanSerialize[T]) =
  safeWriteObject(out, xs)

@main def Test(out: java.io.ObjectOutputStream) =
  writeList(out, List(List("a", "b")))
  writeList(out, List[Int => Int](x => x + 1, y => y * 2)) // error

