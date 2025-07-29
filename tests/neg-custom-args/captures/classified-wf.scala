import caps.*

class Async extends Capability

class IO extends Capability, Classifier

def foo(x: Object^{cap.only[Async]}) = ??? // error
def bar(x: Object^{cap.only[IO]}) = ??? // ok
