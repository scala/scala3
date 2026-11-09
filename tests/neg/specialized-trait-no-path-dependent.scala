//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits
trait T1:
    inline trait Foo[T: Specialized]: // error: May not define specialized traits inside traits or classes so as to make them path-dependent
        def bar = "Bar"
