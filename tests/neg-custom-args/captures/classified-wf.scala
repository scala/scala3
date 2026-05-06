import caps.*

class Async extends SharedCapability

class IO extends SharedCapability, Classifier

def foo(x: Object^{any.only[Async]}) = ??? // error
def bar(x: Object^{any.only[IO]}) = ??? // ok
