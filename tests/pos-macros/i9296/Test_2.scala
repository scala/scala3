package a

import scala.language.implicitConversions
import scala.concurrent._

trait CB[T]

given myConversion[T]: Conversion[Future[T],CB[T]] = (ft => ???)

object O {

  def main(argvs: Array[String]): Unit = {
    val p = Promise[Int]()
    //val cbp = summon[Conversion[Future[Int],CB[Int]]] //works
    val cbp = M.resolveInMacros[CB,Int](p.future)
    val x = cbp(p.future)
  }

}
