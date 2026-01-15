class E extends caps.ExclusiveCapability
class S extends caps.SharedCapability

def par(x: E^, y: E^): E = ???

def test(x: E^) = par(x, x) // error

