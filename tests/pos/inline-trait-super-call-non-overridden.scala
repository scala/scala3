//> using options -language:experimental.inlineTraits
inline trait A:
    def foo = 10

inline trait B extends A:
    def bar = super.foo
