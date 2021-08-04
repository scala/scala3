class Test(i: Int):
  val `<init>` = "init" // error: Illegal backquoted identifier: `<init>` and `<clinit>` are forbidden
  val `<clinit>` = "clinit" // error: Illegal backquoted identifier: `<init>` and `<clinit>` are forbidden
  class `<init>`: // error: Illegal backquoted identifier: `<init>` and `<clinit>` are forbidden
    def `<init>`(in: String) = ??? // error: Illegal backquoted identifier: `<init>` and `<clinit>` are forbidden
  class `<clinit>`: // error: Illegal backquoted identifier: `<init>` and `<clinit>` are forbidden
    def `<clinit>`(in: String) = ??? // error: Illegal backquoted identifier: `<init>` and `<clinit>` are forbidden
