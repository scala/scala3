trait A {
    val a: Int match { case Int => this } // error
}