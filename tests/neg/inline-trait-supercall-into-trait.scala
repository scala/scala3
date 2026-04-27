trait A:
    def foo = "A"

inline trait B extends A:
    override def foo = super.foo // error: Inline trait smay not contain superclass references to classes or non-inline traits

class C1 extends B
