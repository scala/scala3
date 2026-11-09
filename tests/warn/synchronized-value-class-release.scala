//> using options -release 17

// ct.sym refers to `jdk.internal.ValueBased` under a different name
// The test harness passes `-Xtarget`, which `-release` overrides. // nopos-warn

def boxedInt(x: java.lang.Integer) =
  x.synchronized {} // warn

def localDate(d: java.time.LocalDate) =
  d.synchronized {} // warn

def optional(o: java.util.Optional[String]) =
  o.synchronized {} // warn

def ok(s: String) =
  s.synchronized {}
