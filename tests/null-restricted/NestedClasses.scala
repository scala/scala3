object Outer:
    class Inner
    class Holder:
        def inner: List[Outer.Inner] = List[Outer.Inner](new Inner)