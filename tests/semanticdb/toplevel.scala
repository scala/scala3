inline val a = ""
def (x: Int) combine (y: Int) = x + y
given [A](any: A) {
  def sayHello = s"Hello, I am $any"
}
val hello1 = 1.sayHello
