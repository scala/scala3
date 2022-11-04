import compiletime.ops.int.+

class Test:
    val int: Int = 5

    val int2: Int = 6

    val singletonsSum: int.type + int2.type = ???

    val intsSum: Int + Int = ???

    val deepSingletonsSum:
        ((int.type + int2.type) + (int.type + int2.type)) + ((int.type + int2.type) + (int.type + int2.type)) +
        ((int.type + int2.type) + (int.type + int2.type)) + ((int.type + int2.type) + (int.type + int2.type)) = ???

    val deepIntsSum:
        ((Int + Int) + (Int + Int)) + ((Int + Int) + (Int + Int)) +
        ((Int + Int) + (Int + Int)) + ((Int + Int) + (Int + Int)) = ???

    val singletonsIntersection: int.type & int2.type = ???

    val singletonsUnion: int.type | int2.type = ???
