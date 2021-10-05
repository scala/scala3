
class Chain{
    type Tail <: Chain
}

def f[C1 <: Chain](c1: C1)[C2 <: c1.Tail](c2: C2)[C3 <: c2.Tail](c3: C3): c3.Tail = ???