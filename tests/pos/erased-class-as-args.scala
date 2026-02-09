import language.experimental.erasedDefinitions

class A extends compiletime.Erased

class B(val x: Int) extends A

type T = (x: A, y: Int) => Int

type TSub[-T <: A] = (erased x: T, y: Int) => Int

def useT(f: T) = f(new A, 5)

def useTSub(f: TSub[B]) = f(new B(5), 5)

@main def Test() =
  val tInfer = (x: A, y: Int) => y + 1
  val tExpl: T = (x, y) => y + 1
  assert(useT((erased x, y) => y + 1) == 6)
  assert(useT(tInfer) == 6)
  assert(useT(tExpl) == 6)

  val tSub: TSub[A] = (x, y) => y + 1
  assert(useT(tSub) == 6)
  assert(useTSub(tSub) == 6)
