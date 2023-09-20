import scala.annotation.experimental

@experimental class myExperimentalAnnot extends scala.annotation.Annotation

@myExperimentalAnnot // error
def test1: Unit = ()

@experimental
@myExperimentalAnnot
def test2: Unit = ()

@experimental
class Foo {
  @myExperimentalAnnot
  def test3: Unit = ()

  def test4: Unit = {
    @myExperimentalAnnot
    val f: Unit = ()
    f
  }
}
