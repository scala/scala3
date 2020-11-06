package tests
package inheritedMembers1


class A
{
    def A: String
      = ???
    val B: Int
      = ???
    object X
    trait Z
    given B
    type I = Int
    /*<-*/extension (a: A) /*->*/def extension: String
      = ???
}
