object Test extends dotty.runtime.LegacyApp {
  def test(f: () => Int) = {
    val x = f()
    5
  }

  println(test(() => { println("hi there"); 0 }))
}
