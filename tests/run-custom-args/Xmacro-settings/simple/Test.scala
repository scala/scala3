import x.*

object Test {

   def main(args: Array[String]):Unit =
    assert(M.settingsContains("one"))
    assert(!M.settingsContains("notwo"))
    assert(M.settingsContains("two"))

}
