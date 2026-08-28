//> using options -Yexplicit-nulls
import language.experimental.magic
import language.future
import scala.magic.*

@main def Test =
  println(Ok(null))
  println(Ok(Err("bad")))
  println(Ok("good"))

