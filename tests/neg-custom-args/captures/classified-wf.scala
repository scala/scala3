import caps.*

class Async extends SharedCapability

class IO extends SharedCapability, Classifier

def foo(x: Object^{cap.only[Async]}) = ??? // error
def bar(x: Object^{cap.only[IO]}) = ??? // ok
