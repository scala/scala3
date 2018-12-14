## Design Notes


- Abstract platform operations
- Proxies

  // Foo defined in current run
  class Foo extends IFace1 with IFace2 ...
  new Foo.meth()

  // Bar is precompiled
  class Bar {
    def meth(x: IFace1) = x.meth()
  }

  FooProxy(obj).meth()