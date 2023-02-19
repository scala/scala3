// scalajs: --skip

import concurrent.*
import fiberRuntime.boundary.setName

@main def Test =
  given Scheduler = Scheduler
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
  println("test async:")
  Async.blocking:
    println(x.value)
    println(y.value)
  //println("test choices:")
  //println(TestChoices)


