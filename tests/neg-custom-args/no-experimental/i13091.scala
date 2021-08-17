import annotation.experimental

@experimental class Foo

def test: Unit = new Foo // error: class Foo is marked @experimental ...
