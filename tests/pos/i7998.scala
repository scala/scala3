@main def Test =
  (
    try 1
    catch
      case _: Throwable => 2
  )