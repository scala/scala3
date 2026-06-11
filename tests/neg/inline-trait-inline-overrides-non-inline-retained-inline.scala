inline trait A:
    def x: Int

inline trait B extends A:
    override inline def x = 10 // error: implementation restriction: inline traits cannot have non-local private members. This also means no retained inline methods.

@main def Test = 
    val x: A = new B() {}
    assert(x.x == 10)
