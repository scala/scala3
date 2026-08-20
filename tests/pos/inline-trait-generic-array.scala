//> using options -language:experimental.inlineTraits
import scala.reflect.ClassTag

inline trait Foo[K: ClassTag]:
  var keys: Array[Array[K]] = Array.fill(100){new Array[K](50)}

val m = new Foo[Int]() {}
