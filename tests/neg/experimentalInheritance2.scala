import scala.annotation.experimental

@experimental class A

class B // // error: extension of experimental class A1 must have @experimental annotation
  extends A
