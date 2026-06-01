trait Tr(val y: String){
    val x: String = "Trait.x"
    var z: String = "Trait.z"
}

class C(y: String) extends Tr(y){
    val c1 = "C.c1"
    var c2 = "C.c2"
}


