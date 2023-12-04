def testTuple(tup: Tuple) = tup(0)
def testNonEmptyTuple(tup: NonEmptyTuple) = tup(0)
def testConsUnbound(tup: Any *: Tuple) = tup(0)
def testCons(tup: Any *: EmptyTuple) = tup(0)
