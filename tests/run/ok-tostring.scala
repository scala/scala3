//> using options -Yexplicit-nulls
import language.experimental.errorHandling
import language.future


@main def Test =
  println(Ok(null))
  println(Ok(Err("bad")))
  println(Ok("good"))

