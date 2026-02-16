import annotation.retains
object Test:

  class A
  class B
  class C
  class CTC
  type CT = CTC @retains[caps.any.type]

  def test(ct: CT, dt: CT) =

    def x0: A -> B^{ct} = ???

    def x1: A -> B @retains[ct.type] = ???
    def x2: A -> B -> C @retains[ct.type] = ???
    def x3: A -> () -> B -> C @retains[ct.type] = ???

    def x4: (x: A @retains[ct.type]) -> B -> C = ???

    def x5: A -> (x: B @retains[ct.type]) -> () -> C @retains[dt.type] = ???
    def x6: A -> (x: B @retains[ct.type]) -> (() -> C @retains[dt.type]) @retains[x.type | dt.type] = ???
    def x7: A -> (x: B @retains[ct.type]) -> (() -> C @retains[dt.type]) @retains[x.type] = ???