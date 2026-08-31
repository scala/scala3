//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import language.future
import scala.util.{Ok, Err}

@main def Test =
  println(Ok(null))
  println(Ok(Err("bad")))
  println(Ok("good"))

