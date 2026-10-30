def test =
  objectRef((x: Int) => Foo)
  stableVal((x: Int) => Holder.v)
  dependent((x: Int) => x)
