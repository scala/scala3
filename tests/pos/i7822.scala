class Code {
  type Ctx
}
def foo(o0:Code,o1:Code) = new Code { type Ctx = o0.Ctx & o1.Ctx }
def foo0(o0:Code,o1:Code): Code { type Ctx = o0.Ctx & o1.Ctx } = foo(o0,o1)
