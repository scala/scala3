class Unit
class Boolean
class Byte
class Short
class Int
class Long
class Double
class Float
class Char

def u: Unit = () // error
def bool: Boolean = true // error
def b: Byte = 1: scala.Byte // error
def s: Short = 2: scala.Short // error
def i: Int = 3 // error
def l: Long = 4L // error
def f: Float = 5.6 // error
def d: Double = 6.7d // error
def c: Char = 'a' // error
