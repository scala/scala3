val f: Int => String = _ => "b"
var g: Int => String = (_) => "b"  // workaround
var h: Int => String = _ => "b"    // end of statement expected but '=>' found