import caps.*

class Async extends SharedCapability

class IO extends SharedCapability, Classifier

def foo(x: Object^{any.except[Async]}) = ??? // error
def bar(x: Object^{any.except[IO]}) = ??? // ok
