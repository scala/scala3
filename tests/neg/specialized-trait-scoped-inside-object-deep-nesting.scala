//> using options -language:experimental.specializedTraits
//> using options -language:experimental.specializedTraits

package p1:
    package p2:
        object O1:
            class C1:
                trait T1:
                    object O2:
                        inline trait Foo[T: Specialized]: // error: May not define specialized traits inside traits or classes so as to make them path-dependent
                            def bar = "Bar"

                        class F extends Foo
    
        val v = O1.C1()
        val w = new v.T1 {}
        def foo = new w.O2.Foo[Int] {}

@main def Test = p1.p2.foo.bar
