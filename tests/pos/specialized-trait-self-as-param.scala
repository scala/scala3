//> using options -language:experimental.specializedTraits
inline trait Trait[T: Specialized](x: Trait[T] | Int):
    def do_something(): Unit = 
        assert(Thread.currentThread.getStackTrace()(1).getClassName().contains("$impl$")) 
        if x.isInstanceOf[Trait[T]] then
            x.asInstanceOf[Trait[T]].do_something()
        
@main def Test = 
    val b = new Trait(new Trait(10) {}) {}
    b.do_something()
