abstract class A {

    lazy val lazyBar = bar

    object bar {
        val foo = 12
    }

}

object Test extends dotty.runtime.LegacyApp {
    val a = new A{}
    println(a.lazyBar.foo)
}
