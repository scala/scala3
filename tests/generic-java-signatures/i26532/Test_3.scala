import i26532.Impl

object Test:
  def main(args: Array[String]): Unit =
    val helper = classOf[Impl[?, ?]].getDeclaredMethod("helper")
    println(helper.toGenericString)
