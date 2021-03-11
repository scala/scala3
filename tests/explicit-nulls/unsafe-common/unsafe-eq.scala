val s1: String = ???
val s2: String | Null = ???

def f = {
  s1 eq s2 // error
  s2 eq s1 // error

  s1 ne s2 // error
  s2 ne s1 // error

  s1 eq null // error
  s2 eq null // error

  null eq s1 // error
  null eq s2 // error
}