object Test{
 val a: Nothing = ???
 val b: Null = ???
 val c: Null | Nothing = if(a == b) a else b
 val d: Nothing | Nothing = if(a == b) a else b
}
