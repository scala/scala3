trait Foo:
    def foo = 10

inline trait Bar:
    def foo = 10

class C extends Bar, Foo // error: C inherits conflicting members
class D extends Foo, Bar // error: D inherits conflicting members
