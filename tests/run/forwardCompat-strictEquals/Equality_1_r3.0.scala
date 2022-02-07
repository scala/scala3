// Instances of CanEqual are erased during compilation so their absence at runtime should not cause a crash

import scala.language.strictEquality

def emptyTupleEquality = EmptyTuple == EmptyTuple
