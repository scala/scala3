

import caps.SharedCapability

class Async extends SharedCapability

def test1(a: Async): Object^ = a // OK

def test2(a: Async^): Object^ = a // error

