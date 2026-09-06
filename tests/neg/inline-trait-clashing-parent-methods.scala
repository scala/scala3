//> using options -language:experimental.inlineTraits
inline trait A:
    def foo = "Hello World"

inline trait B:
    def foo = "Bonjour"

class C extends A, B // error: C inherits conflicting members
