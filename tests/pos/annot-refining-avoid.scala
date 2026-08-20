import scala.annotation.RefiningAnnotation

class myAnnot extends RefiningAnnotation

type Refined = Int @myAnnot

def test =
  val result =
    val tmp: Refined = ???
    tmp
  result: Refined
