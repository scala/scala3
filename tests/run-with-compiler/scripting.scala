object Test {
  def main(args: Array[String]): Unit = {
    val m = new javax.script.ScriptEngineManager(getClass().getClassLoader())
    val e = m.getEngineByName("scala")
    println(e.eval("42"))
  }
}

