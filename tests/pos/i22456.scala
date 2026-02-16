import language.experimental.modularity

class T(tracked val y: Int)
class C(tracked val x: Int) extends T(x + 1)
