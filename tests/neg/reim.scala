import dotty.readonly

object reim {
  @readonly def m: Box = { // ERROR 9
    box = new Box // ERROR 1

    val robox = box: @readonly
    robox.field = 5 // ERROR 2

    val rwbox2: Box = robox // ERROR 3

    val rwbox = box

    rwbox.field = 5
    robox.field = 5 // ERROR 4

    val robox2: Box @readonly = robox
    val robox3 = robox

    box.next = new Box
    rwbox.next.field = 5
    robox.next.field = 5 // ERROR 5

    def method(rwbox: Box, robox: Box @readonly) = rwbox
    method(robox, rwbox) // ERROR 6

    var localvar: Box = null
    localvar = box

    {import rwbox.field; field = 5}
    {import robox.field; field = 5} // ERROR 7

    robox // ERROR 8
  }

  class Box {
    var field: Int = 42
    //    var field = 42
    var next: Box = null
  }

  var box: Box = new Box
  //    var box = new Box

}