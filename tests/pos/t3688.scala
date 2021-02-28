import collection.mutable
import collection.convert.ImplicitConversions._
import java.{util => ju}

object Test {
 implicitly[mutable.Map[Int, String] => ju.Dictionary[Int, String]]
}

object Test2 {
  def m[P <% ju.List[Int]](l: P) = { val a = l.as[ju.List[Int]] ; 1 }
  m(List(1))
}
