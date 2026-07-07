//> using options -language:experimental.erasedDefinitions

class Proof
erased given Proof = Proof()

class P1
class P2
erased given P1 = P1()
erased given P2 = P2()

def mixed: (erased p: Proof) ?=> Int ?=> String =
  s"value: ${summon[Int]}"

def reversed: Int ?=> (erased p: Proof) ?=> String =
  s"value: ${summon[Int]}"

def varA: (erased p1: P1) ?=> (erased p2: P2) ?=> Int ?=> String =
  s"varA: ${summon[Int]}"

def varB: (erased p1: P1) ?=> Int ?=> (erased p2: P2) ?=> String =
  s"varB: ${summon[Int]}"

def varC: Int ?=> (erased p1: P1) ?=> String ?=> String =
  s"varC: ${summon[Int]} ${summon[String]}"

@main def Test(): Unit =
  given Int = 99
  given String = "ok"
  println(mixed)
  println(reversed)
  println(varA)
  println(varB)
  println(varC)
