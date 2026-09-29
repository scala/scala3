import caps.{any, Classifier, Control, SharedCapability}

trait IOCap extends SharedCapability, Classifier

class File extends IOCap:
  def read(): Int = 1

def runOnNewThread[T](body: () ->{any.except[Control]} T): T = body()

def restricted(c: Object^): Object^{c.only[SharedCapability].except[Control]} = ???

def readOnlyPart(c: Object^): Object^{c.except[Control].rd} = ???
