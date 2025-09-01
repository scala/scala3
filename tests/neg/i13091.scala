

import annotation.experimental

@experimental class Foo

def test = (new Foo): Unit // error: class Foo is marked @experimental ...
