import scala.annotation.experimental

@experimental // error
class myExperimentalAnnot extends scala.annotation.Annotation

@myExperimentalAnnot // error
def test: Unit = ()
