import scala.annotation.experimental

@experimental class myExperimentalAnnot extends scala.annotation.Annotation

@myExperimentalAnnot // error
def test1: Unit = ()

@experimental
@myExperimentalAnnot
def test2: Unit = ()
