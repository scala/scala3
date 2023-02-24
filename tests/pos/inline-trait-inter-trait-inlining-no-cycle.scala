//> using options -language:experimental.inlineTraits
inline trait F:
   def i = println("HELLO WORLD")

inline trait E:
   def h = 
      new F() {}

inline trait D:
   def g = 
      new E() {}

inline trait C:
   def f = 
      new D() {}

class MyClass extends C, D, E, F
