def selfie0: (AnyRef => AnyRef) => AnyRef = (f:AnyRef => AnyRef) => f(f)
def selfie1: Any = (f: Any => Any) => f(f)
def selfie2: Any = (f: (Any, Any) => Any, g: (Any, Any) => Any) => f(f, g)
def selfie3: Any = (f: (Any, Any) => Any, g: (Any, Any) => Any) => g(f, g)
