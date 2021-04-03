class C {
    def get(): Int = 0
}

def g = {
    val s: String | Null = ???
    val l = s.length // ok
    val c: C | Null = ???
    c.get() // error: value get is not a member of C | Null
}