    type Proc = () -> Unit
     def foo(f: Proc^): Proc^{} =
       def bar[A <: Proc^{f}](g: () -> A): () -> Proc^{f} =
         g  // error
       val stowaway: () -> Proc^{f} =
         bar( () => f )  // error
       () => { stowaway.apply().apply() }