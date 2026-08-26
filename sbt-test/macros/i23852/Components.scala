object Components extends App {
  try {
    wire[Dep]
  } catch {
    case e: Throwable =>
      e.printStackTrace()
      sys.exit(-1)
  }
}
