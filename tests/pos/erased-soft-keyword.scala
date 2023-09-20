//> using options -experimental -language:experimental.erasedDefinitions

def f1(x: Int, erased y: Int) = 0
def f2(x: Int, erased: Int) = 0
inline def f3(x: Int, inline erased: Int) = 0
def f4(x: Int, erased inline: Int) = 0
// inline def f5(x: Int, erased inline y: Int) = 0 // should parse but rejected later

def f6(using erased y: Int) = 0
def f7(using erased: Int) = 0
inline def f8(using inline erased: Int) = 0
def f9(using erased inline: Int) = 0
// inline def f10(using erased inline x: Int) = 0 // should parse but rejected later
def f11(using erased Int) = 0

val v1 = (erased: Int) => 0
val v2: Int => Int = erased => 0
val v3 = (erased x: Int) => 0
val v4: (erased Int) => Int = (erased x) => 0
val v5: (erased: Int) => Int = x => 0
