// See also tests/pos/annot-21595.scala

class local(predicate: Int) extends annotation.StaticAnnotation

def failing1(x: Int, z: Int @local(x + x)) = ()
