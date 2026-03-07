//> using options -Ydebug
package i17445

def complileMe:Int =
  Macro.changeIndexWhere {
    val arr = Array(1, 2, 3)
    val result = arr.indexWhere(_ == 2)
    result
  }