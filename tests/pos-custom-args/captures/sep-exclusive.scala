class E extends caps.ExclusiveCapability
class S extends caps.SharedCapability

def par(x: S^, y: S^): S = ???

def test(x: S^) = par(x, x)

