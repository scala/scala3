package dotty.tools
package dotc
package transform


class LazyValsTest extends DottyTest {
    /* FIXME: re-enable after adapting to new scheme
    @Test
    def doNotRewriteObjects = {
      checkCompile("LazyVals", "object O"){ (tree, ctx) =>
        Assert.assertTrue("local lazy shouldn't rewrite module instance definitions", tree.toString.contains(
          "ValDef(Modifiers(final module <stable>,,Vector()),O,"
        ))
      }
    }

    @Test
    def localInt = {
      checkCompile("LazyVals", "class LocalLV { def m = { lazy val s = 1;  s }}"){ (tree, ctx) =>
        Assert.assertTrue("local lazy int rewritten to class creation", tree.toString.contains(
        "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyInt)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyInt)]),<init>),Vector(Literal(Constant(1)))))"
        ))
      }
    }

      @Test
      def localLong = {
        checkCompile("LazyVals", "class LocalLV { def m = { lazy val s = 1L;  s }}"){ (tree, ctx) =>
          Assert.assertTrue("local lazy long rewritten to class creation", tree.toString.contains(
            "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyLong)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyLong)]),<init>),Vector(Literal(Constant(1)))))"
          ))
        }
      }

      @Test
      def localFloat = {
        checkCompile("LazyVals", "class LocalLV { def m = { lazy val s = 1.0f;  s }}"){ (tree, ctx) =>
          Assert.assertTrue("local lazy float rewritten to class creation", tree.toString.contains(
            "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyFloat)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyFloat)]),<init>),Vector(Literal(Constant(1.0)))))"
          ))
        }
      }

      @Test
      def localDouble = {
        checkCompile("LazyVals", "class LocalLV { def m = { lazy val s = 1.0;  s }}"){ (tree, ctx) =>
          Assert.assertTrue("local lazy double rewritten to class creation", tree.toString.contains(
            "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyDouble)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyDouble)]),<init>),Vector(Literal(Constant(1.0)))))"
          ))
        }
      }

      @Test
      def localBoolean = {
        checkCompile("LazyVals", "class LocalLV { def m = { lazy val s = true;  s }}"){ (tree, ctx) =>
          Assert.assertTrue("local lazy boolean rewritten to class creation", tree.toString.contains(
            "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyBoolean)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyBoolean)]),<init>),Vector(Literal(Constant(true)))))"
          ))
        }
      }

      @Test
      def localChar = {
        checkCompile("LazyVals", "class LocalLV { def m = { lazy val s = 'a';  s }}"){ (tree, ctx) =>
          Assert.assertTrue("local lazy char rewritten to class creation", tree.toString.contains(
            "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyChar)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyChar)]),<init>),Vector(Literal(Constant(a)))))"
          ))
        }
      }

      @Test
      def localByte = {
        checkCompile("LazyVals", "class LocalLV { def m = { lazy val s:Byte = 1;  s }}"){ (tree, ctx) =>
          Assert.assertTrue("local lazy byte rewritten to class creation", tree.toString.contains(
            "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyByte)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyByte)]),<init>),Vector(Literal(Constant(1)))))"
          ))
        }
      }

      @Test
      def localShort = {
        checkCompile("LazyVals", "class LocalLV { def m = { lazy val s:Short = 1;  s }}"){ (tree, ctx) =>
          Assert.assertTrue("local lazy short rewritten to class creation", tree.toString.contains(
            "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyShort)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyShort)]),<init>),Vector(Literal(Constant(1)))))"
          ))
        }
      }

      @Test
      def localRef = {
        checkCompile("LazyVals", "class LocalLV { def m = { lazy val s = \"string\";  s }}"){ (tree, ctx) =>
          Assert.assertTrue("local lazy ref rewritten to class creation", tree.toString.contains(
            "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class runtime),LazyRef)],Apply(Select(New(TypeTree[TypeRef(ThisType(module class runtime),LazyRef)]),<init>),Vector(Literal(Constant(string)))))"
          ))
        }
      }

      @Test
      def fieldRef = {
        checkCompile("LazyVals", "class LV { lazy val s = \"string\" }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy int rewritten to class creation", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class lang),String)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(string)))),Ident(s$lzy1))))"
          ))
        }
      }

      @Test
      def fieldInt = {
        checkCompile("LazyVals", "class LV { lazy val s = 1 }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy int rewritten", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Int)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(1)))),Ident(s$lzy1))))"
          ))
        }
      }

      @Test
      def fieldLong = {
        checkCompile("LazyVals", "class LV { lazy val s = 1L }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy long rewritten", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Long)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(1)))),Ident(s$lzy1))))"
          ))
        }
      }

      @Test
      def fieldShort = {
        checkCompile("LazyVals", "class LV { lazy val s:Short = 1 }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy short rewritten", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Short)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(1)))),Ident(s$lzy1))))"
          ))
        }
      }

      @Test
      def fieldByte = {
        checkCompile("LazyVals", "class LV { lazy val s:Byte = 1 }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy byte rewritten", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Byte)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(1)))),Ident(s$lzy1))))"
          ))
        }
      }

      @Test
      def fieldBoolean = {
        checkCompile("LazyVals", "class LV { lazy val s = true }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy boolean rewritten", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Boolean)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(true)))),Ident(s$lzy1))))"
          ))
        }
      }

      @Test
      def fieldDouble = {
        checkCompile("LazyVals", "class LV { lazy val s = 1.0 }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy double rewritten", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Double)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(1.0)))),Ident(s$lzy1))))"
          ))
        }
      }

      @Test
      def fieldFloat = {
        checkCompile("LazyVals", "class LV { lazy val s = 1.0f }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy float rewritten", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Float)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(1.0)))),Ident(s$lzy1))))"
          ))
        }
      }

      @Test
      def fieldChar = {
        checkCompile("LazyVals", "class LV { lazy val s = 'a' }"){ (tree, ctx) =>
          Assert.assertTrue("field lazy char rewritten", tree.toString.contains(
            "DefDef(Modifiers(,,Vector()),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Char)],If(Ident(sbitmap$1),Ident(s$lzy1),Block(Vector(Assign(Ident(sbitmap$1),Literal(Constant(true))), Assign(Ident(s$lzy1),Literal(Constant(a)))),Ident(s$lzy1))))"
          ))
        }
      }

    @Test
    def volatileFieldRef = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s = \"a\" }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class lang),String)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(ThisType(module class lang),String)],Literal(Constant(null))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(a))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class lang),String)],Literal(Constant(null))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val treeS = tree.toString
          //println(treeS)
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatileFieldInt = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s = 1 }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(ThisType(module class scala),Int)],Literal(Constant(0))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(1))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class scala),Int)],Literal(Constant(0))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatileFieldLong = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s = 1L }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Long)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(1))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatileFieldFloat = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s = 1.0f }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Float)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(ThisType(module class scala),Float)],Literal(Constant(0.0))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(1.0))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class scala),Float)],Literal(Constant(0.0))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatileFieldDouble = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s = 1.0 }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Double)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(ThisType(module class scala),Double)],Literal(Constant(0.0))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(1.0))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class scala),Double)],Literal(Constant(0.0))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatileFieldBoolean = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s = true }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Boolean)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(false))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(true))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(false))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatileFieldByte = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s:Byte = 1 }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Byte)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Byte)],Literal(Constant(0))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(1))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Byte)],Literal(Constant(0))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatileFieldShort = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s:Short = 1 }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Short)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Short)],Literal(Constant(0))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(1))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(TermRef(ThisType(module class <root>),scala),Short)],Literal(Constant(0))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatileFieldChar = {
      checkCompile("LazyVals", "class LV { @volatile lazy val s = 'a' }") {
        (tree, ctx) =>
          val accessor = "DefDef(Modifiers(,,Vector(Apply(Select(New(Ident(volatile)),<init>),Vector()))),s,Vector(),Vector(),TypeTree[TypeRef(ThisType(module class scala),Char)],Block(Vector(ValDef(Modifiers(,,Vector()),result,TypeTree[TypeRef(ThisType(module class scala),Char)],Literal(Constant(\u0000))), ValDef(Modifiers(,,Vector()),retry,TypeTree[TypeRef(ThisType(module class scala),Boolean)],Literal(Constant(true))), ValDef(Modifiers(,,Vector()),flag,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0))), WhileDo(Ident(retry),Block(Vector(Assign(Ident(flag),Apply(Select(Ident(LazyVals),get),Vector(This(LV), Select(Ident(LV),OFFSET$0))))),Match(Apply(Select(Ident(LazyVals),STATE),Vector(Ident(flag), Literal(Constant(0)))),Vector(CaseDef(Literal(Constant(0)),EmptyTree,If(Apply(Select(Ident(LazyVals),CAS),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(1)), Literal(Constant(0)))),Block(Vector(Try(Assign(Ident(result),Literal(Constant(a))),Block(Vector(DefDef(Modifiers(,,Vector()),$anonfun,Vector(),Vector(Vector(ValDef(Modifiers(,,Vector()),x$1,TypeTree[TypeRef(ThisType(module class lang),Throwable)],EmptyTree))),TypeTree[TypeRef(ThisType(module class scala),Int)],Block(Vector(Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(0)), Literal(Constant(0))))),Throw(Ident(x$1))))),Closure(Vector(),Ident($anonfun),EmptyTree)),EmptyTree), Assign(Ident(s$lzy1),Ident(result)), Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(0)))), Assign(Ident(retry),Literal(Constant(false)))),Literal(Constant(()))),Literal(Constant(())))), CaseDef(Literal(Constant(1)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(2)),EmptyTree,Apply(Select(Ident(LazyVals),wait4Notification),Vector(This(LV), Select(Ident(LV),OFFSET$0), Ident(flag), Literal(Constant(0))))), CaseDef(Literal(Constant(3)),EmptyTree,Block(Vector(Assign(Ident(retry),Literal(Constant(false))), Assign(Ident(result),Ident(s$lzy1))),Literal(Constant(()))))))))),Ident(result)))"
          val fields = "ValDef(Modifiers(,,Vector()),s$lzy1,TypeTree[TypeRef(ThisType(module class scala),Char)],Literal(Constant(\u0000))), ValDef(Modifiers(,,Vector()),bitmap$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Literal(Constant(0)))"
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"

          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(accessor) && treeS.contains(fields) && treeS.contains(moduleField))
      }
    }

    @Test
    def volatilesReuseBitmaps = {
      checkCompile("LazyVals", "class LV { @volatile lazy val a = 'a'; @volatile lazy val b = 'b';  }") {
        (tree, ctx) =>
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val reuseFieldPattern = "Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$0), Literal(Constant(3)), Literal(Constant(1))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation\n" + treeS,
            treeS.contains(moduleField) && treeS.contains(reuseFieldPattern))
      }
    }

    @Test
    def volatilesCreateNewBitmaps = {
      checkCompile("LazyVals",
        """
          | class LV {
          |   @volatile lazy val a1 = '1';
          |   @volatile lazy val a2 = '1';
          |   @volatile lazy val a3 = '1';
          |   @volatile lazy val a4 = '1';
          |   @volatile lazy val a5 = '1';
          |   @volatile lazy val a6 = '1';
          |   @volatile lazy val a7 = '1';
          |   @volatile lazy val a8 = '1';
          |   @volatile lazy val a9 = '1';
          |   @volatile lazy val a10 = '1';
          |   @volatile lazy val a11 = '1';
          |   @volatile lazy val a12 = '1';
          |   @volatile lazy val a13 = '1';
          |   @volatile lazy val a14 = '1';
          |   @volatile lazy val a15 = '1';
          |   @volatile lazy val a16 = '1';
          |   @volatile lazy val a17 = '1';
          |   @volatile lazy val a18 = '1';
          |   @volatile lazy val a19 = '1';
          |   @volatile lazy val a20 = '1';
          |   @volatile lazy val a21 = '1';
          |   @volatile lazy val a22 = '1';
          |   @volatile lazy val a23 = '1';
          |   @volatile lazy val a24 = '1';
          |   @volatile lazy val a25 = '1';
          |   @volatile lazy val a26 = '1';
          |   @volatile lazy val a27 = '1';
          |   @volatile lazy val a28 = '1';
          |   @volatile lazy val a29 = '1';
          |   @volatile lazy val a30 = '1';
          |   @volatile lazy val a31 = '1';
          |   @volatile lazy val a32 = '1';
          |   @volatile lazy val a33 = '1';
          |   @volatile lazy val a34 = '1';
          | }
        """.stripMargin ){
        (tree, ctx) =>
          val moduleField = "TypeDef(Modifiers(final module <synthetic>,,Vector()),LV$,Template(DefDef(Modifiers(,,Vector()),<init>,Vector(),Vector(Vector()),TypeTree[TypeRef(ThisType(module class <empty>),LV$)],EmptyTree),Vector(Apply(Select(New(TypeTree[TypeRef(ThisType(module class lang),Object)]),<init>),Vector())),ValDef(Modifiers(,,Vector()),_,TypeTree[TermRef(ThisType(module class <empty>),LV)],EmptyTree),Vector(ValDef(Modifiers(,,Vector()),OFFSET$1,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$1))))), ValDef(Modifiers(,,Vector()),OFFSET$0,TypeTree[TypeRef(ThisType(module class scala),Long)],Apply(Select(Ident(LazyVals),getOffset),Vector(This(LV), Literal(Constant(bitmap$0))))))))"
          val reuseFieldPattern = "Apply(Select(Ident(LazyVals),setFlag),Vector(This(LV), Select(Ident(LV),OFFSET$1), Literal(Constant(3)), Literal(Constant(1))))"
          val treeS = tree.toString
          Assert.assertTrue("volatile field lazy ref rewritten to class creation",
            treeS.contains(moduleField) && treeS.contains(reuseFieldPattern))
      }
    }*/
}
