//> using options -Yno-deep-subtypes

object Record:
  opaque type Record[F] = Unit

  extension [F](record: Record[F])
    def &[A](other: Record[A]): Record[F & A] = ()

  class has[Name, Value]()

  extension (self: String)
    def ~[Value](value: Value): Record[has[self.type, Value]] = ()

import Record.~

val r = "f1" ~ 1 & "f2" ~ "hello" & "f3" ~ true & "f4" ~ 4 & "f5" ~ 5 &
  "f6" ~ 6 & "f7" ~ 7 & "f8" ~ 8 & "f9" ~ 9 & "f10" ~ 10 &
  "f11" ~ 11 & "f12" ~ 12 & "f13" ~ 13 & "f14" ~ 14 & "f15" ~ 15 &
  "f16" ~ 16 & "f17" ~ 17 & "f18" ~ 18 & "f19" ~ 19 & "f20" ~ 20 &
  "f21" ~ 21 & "f22" ~ 22 & "f23" ~ 23 & "f24" ~ 24 & "f25" ~ 25
