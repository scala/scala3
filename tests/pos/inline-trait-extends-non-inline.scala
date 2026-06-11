trait A:
    def foo = "Hello World"

inline trait B extends A

trait C extends B
