@main def Test = {
  def result = for {
    a <- Option(2)
    _ = if (true) {
      sys.error("err")
    }
  } yield a

  try
    result
    ???
  catch case e: RuntimeException => assert(e.getMessage == "err")
}
