class C
import caps.unsafe.*

def foo(x: C^): C = x.unsafeAssumePure
