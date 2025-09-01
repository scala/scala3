package defaultArgBug

class staticAnnot(arg: Int) extends scala.annotation.StaticAnnotation
class refiningAnnot(arg: Int) extends scala.annotation.RefiningAnnotation

def f1(a: Int, b: Int @staticAnnot(a + a) = 42): Int = b
def f2(a: Int, b: Int @refiningAnnot(a + a) = 42): Int = b
