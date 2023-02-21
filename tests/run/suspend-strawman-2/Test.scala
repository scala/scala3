// scalajs: --skip

import concurrent.*
import fiberRuntime.boundary.setName
import scala.concurrent.ExecutionContext

@main def Test =
  given ExecutionContext = ExecutionContext.global
  val x = Future:
    setName("x")
    val a = Future{ setName("xa"); 22 }
    val b = Future{ setName("xb"); 11 }
    val c = Future { setName("xc"); assert(false); 1 }
    c.alt(Future{ setName("alt1"); a.value + b.value }).alt(c).value
  val y = Future:
    setName("y")
    val a = Future{ setName("ya"); 22 }
    val b = Future{ setName("yb"); 11 }
    a.zip(b).value
  val z = Future:
    val a = Future{ setName("za"); 22 }
    val b = Future{ setName("zb"); true }
    a.alt(b).value
  val _: Future[Int | Boolean] = z
  println("test async:")
  Async.blocking:
    println(x.value)
    println(y.value)
  //println("test choices:")
  //println(TestChoices)



