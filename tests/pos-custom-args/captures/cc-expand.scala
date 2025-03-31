import annotation.retains
object Test:

  class A
  class B
  class C
  class CTC
  type CT = CTC @retains[caps.cap.type]

  def test(ct: CT, dt: CT) =

    def x0: A -> B^{ct} = ???

    def x1: A -> B @retains(ct) = ???
    def x2: A -> B -> C @retains(ct) = ???
    def x3: A -> () -> B -> C @retains(ct) = ???

    def x4: (x: A @retains(ct)) -> B -> C = ???

    def x5: A -> (x: B @retains[ct.type]) -> () -> C @retains(dt) = ???
    def x6: A -> (x: B @retains[ct.type]) -> (() -> C @retains[dt.type]) @retains[x.type | dt.type] = ???
    def x7: A -> (x: B @retains[ct.type]) -> (() -> C @retains[dt.type]) @retains[x.type] = ???