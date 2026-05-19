//> using options -language:experimental.erasedDefinitions

class Proof
erased given Proof = Proof()

class P1
class P2
erased given P1 = P1()
erased given P2 = P2()

def mixed: (erased Proof) ?=> Int ?=> String =
  s"value: ${summon[Int]}"

def reversed: Int ?=> (erased Proof) ?=> String =
  s"value: ${summon[Int]}"

def varA: (erased P1) ?=> (erased P2) ?=> Int ?=> String =
  s"varA: ${summon[Int]}"

def varB: (erased P1) ?=> Int ?=> (erased P2) ?=> String =
  s"varB: ${summon[Int]}"

def varC: Int ?=> (erased P1) ?=> String ?=> String =
  s"varC: ${summon[Int]} ${summon[String]}"

@main def Test(): Unit =
  given Int = 99
  given String = "ok"
  println(mixed)
  println(reversed)
  println(varA)
  println(varB)
  println(varC)
