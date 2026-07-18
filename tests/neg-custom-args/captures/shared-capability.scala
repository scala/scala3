

import caps.Sharable

class Async extends Sharable

def test1(a: Async): Object^ = a // OK

def test2(a: Async^): Object^ = a // error

