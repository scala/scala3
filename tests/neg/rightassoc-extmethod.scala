extension (x: Int) def +: (using String): Int = x // error
extension (x: Int) def *: (y: Int, z: Int) = x // error

