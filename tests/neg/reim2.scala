import dotty.readonly

object reim2 {
  @readonly def m: Box = {
    val rwbox = new Box
    box = rwbox // ERROR 1 ReimPhase

    val robox = rwbox: @readonly
    robox.field = 5 // ERROR 2 ReimPhase

//    val rwbox2: Box = robox // ERROR 3 ReimTyper


    rwbox.field = 5
    robox.field = 5 // ERROR 4 ReimPhase

    val robox2: Box @readonly = robox
    val robox3 = robox

    box.next = new Box // ERROR 10 ReimPhase
    rwbox.next = new Box
    rwbox.next.field = 5
    robox.next.field = 5 // ERROR 5 ReimPhase

    def method(rwbox: Box, robox: Box @readonly) = rwbox
//    method(robox, rwbox) // ERROR 6 ReimTyper

    var localvar: Box = null
//    localvar = box // ERROR 9 ReimTyper

    {import rwbox.field; field = 5}
    {import robox.field; field = 5} // ERROR 7 ReimPhase

//    robox // ERROR 8 ReimTyper
    rwbox
  }

  class Box {
    var field: Int = 42
    //    var field = 42
    var next: Box = null
  }

  var box: Box = new Box
  //    var box = new Box

}
