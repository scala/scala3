val _ = (new Function[(Int, Int), Int] {def apply(a: Int, b: Int): Int = a * b})(2, 3) // error
