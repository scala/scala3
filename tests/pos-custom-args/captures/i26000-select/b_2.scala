package i26000select
import language.experimental.captureChecking

object Use:
  def flag = true
  val a1 = (if flag then Lib.gb else Lib.hb).value
  val a2 = (if flag then Lib.gc else Lib.hc).value
  val a3 = (if flag then Lib.gt else Lib.ht)._1
  val a4 = (flag match { case true => Lib.gc; case _ => Lib.hc }).value
  val a6 = List(Lib.g, Lib.h).head
  val a7 = List(Lib.gc, Lib.hc).head.value
  val a8 = (try Lib.gc finally ()).value
  val a9 = (if flag then CBox(Lib.g) else CBox(Lib.h)).value
  def local() =
    val t = if flag then Lib.gc else Lib.hc
    val m = t.value
    m
