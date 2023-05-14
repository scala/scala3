val s1: String = ???
val s2: String | Null = ???

def f = {
  s1 eq s2
  s2 eq s1

  s1 ne s2
  s2 ne s1

  s1 eq null
  s2 eq null

  null eq s1
  null eq s2
}
