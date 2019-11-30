package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait StringOps extends Core with

  given StringOps: (string: String) with
    def toTermName: TermName = internal.String_toTermName(string)

  given StringContextOps: (stringContext: StringContext) with
    def i(args: => Any*)(given Context): String = internal.StringContext_i(stringContext, args)
