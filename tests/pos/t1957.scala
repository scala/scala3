// See comment at end of file.
object Test {
    abstract class Settings {}

    abstract class Grist
    { self =>
        type settingsType <: Settings
        type moduleType <: Module {type settingsType = self.settingsType}
        val module: moduleType
    }

    abstract class Tool
    { self =>
        type settingsType <: Settings
        type moduleType = Module { type settingsType = self.settingsType }
        type gristType = Grist { type moduleType <: self.moduleType; type settingsType <: self.settingsType }

        def inputGrist: List[gristType]
    }

    abstract class Module
    { self =>
        type settingsType <: Settings
        final type commonModuleType = Module {type settingsType = self.settingsType}
        type selfType >: self.type <: commonModuleType

        // BTW: if we use the commented out type decls, the code compiles successfully
        // type gristType = Grist {type settingsType <: self.settingsType; type moduleType <: commonModuleType }

        val tools: List[Tool {type settingsType = self.settingsType}]

        protected def f: List[commonModuleType] =
        {
            val inputGrists = tools.flatMap(_.inputGrist)
              // This produces an unhygienic closure for _.inputGrist.
              // Pickling will log:
              //
              // [...] pickling reference to as yet undefined value _$1 in method $anonfun
              //
              // More info can be produced by uncommenting these two lines in
              // Namer#valOrDefDefSig:
              //
              //println(i"lifting $rhsType over $paramss -> $hygienicType = ${tpt.tpe}")
              //println(TypeComparer.explained { implicit ctx => hygienicType <:< tpt.tpe })
              //
              // Tracing the subtype statement (over 1600+ lines!) shows that the TypeComparer thinks that the
              // following subtype judgement is true:
              //
              //    Test.Grist{
              //      moduleType <: Test.Module{settingsType = Module.this.settingsType};
              //      settingsType <: Module.this.settingsType
              //    } <:< Test.Grist{moduleType <: _$1.moduleType; settingsType <: _$1.settingsType}
              //
              // Therefore, a type variable which has the second type as lower bound does not get
              // the (hygienic) first type as new lower bound. Clearly something is wrong in the subtype
              // derivation here. It would be important to figure out what.

              ???
//            inputGrists.map(_.module)
        }

    }
}
