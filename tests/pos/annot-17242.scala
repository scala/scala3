class local(predicate: Int) extends annotation.StaticAnnotation

def failing1(x: Int, z: Int @local(x + x)) = ()
