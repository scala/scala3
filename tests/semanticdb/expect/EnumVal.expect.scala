package enumVal

import scala.runtime.EnumValue/*->scala::runtime::EnumValue.*/


trait A/*<-enumVal::A#*/

enum Color/*<-enumVal::Color#*/(val rgb/*<-enumVal::Color#rgb.*/: Int/*->scala::Int#*/):
  case Red/*<-enumVal::Color.Red.*/   extends Color/*->enumVal::Color#*/(0xFF0000) with EnumValue/*->scala::runtime::EnumValue#*/
  case Green/*<-enumVal::Color.Green.*/ extends Color/*->enumVal::Color#*/(0x00FF00) with A/*->enumVal::A#*/
  case Blue/*<-enumVal::Color.Blue.*/  extends Color/*->enumVal::Color#*/(0x0000FF)
