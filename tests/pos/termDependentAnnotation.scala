
class local(predicate: Boolean) extends annotation.StaticAnnotation
def f(x: Int, z: Int @local(x == x)) = ???
