package a

import scala.language.implicitConversions
import scala.concurrent.*

trait CB[T]


object O {

  given myConversion[T]: Conversion[Future[T],CB[T]] = (ft => ???)

  def main(argvs: Array[String]): Unit = {
    val p = Promise[Int]()
    //val cbp = summon[Conversion[Future[Int],CB[Int]]] //works
    val cbp = M.resolveInMacros[CB,Int](p.future)
    val x = cbp(p.future)
  }

}
