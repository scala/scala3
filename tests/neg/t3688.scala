import collection.mutable
import collection.convert.ImplicitConversions._
import java.{util => ju}

object Test {
 implicitly[mutable.Map[Int, String] => ju.Dictionary[Int, String]]
}

object Test2 {
  def m[P <% ju.List[Int]](l: P) = { val a: ju.List[Int] = l ; 1 } // error: found P required ju.List[Int]
  m(List(1))
}
