package dotty.tools.dotc
package config

import collection.mutable.{ ArrayBuffer }
import scala.util.{ Try, Success, Failure }
import reflect.ClassTag
import core.Contexts._
import scala.annotation.tailrec
// import annotation.unchecked
  // Dotty deviation: Imports take precedence over definitions in enclosing package
  // (Note that @unchecked is in scala, not annotation, so annotation.unchecked gives
  // us a package, which is not what was intended anyway).
import language.existentials

object Settings {

  val BooleanTag = ClassTag.Boolean
  val IntTag = ClassTag.Int
  val StringTag = ClassTag(classOf[String])
  val ListTag = ClassTag(classOf[List[_]])
  val VersionTag = ClassTag(classOf[ScalaVersion])
  val OptionTag = ClassTag(classOf[Option[_]])

  class SettingsState(initialValues: Seq[Any]) {
    private var values = ArrayBuffer(initialValues: _*)
    private var _wasRead: Boolean = false

    override def toString = s"SettingsState(values: ${values.toList})"

    def value(idx: Int): Any = {
      _wasRead = true
      values(idx)
    }

    def update(idx: Int, x: Any): SettingsState =
      if (_wasRead)
        new SettingsState(values).update(idx, x)
      else {
        values(idx) = x
        this
      }
  }

  case class ArgsSummary(
    sstate: SettingsState,
    arguments: List[String],
    errors: List[String],
    warnings: List[String]) {

    def fail(msg: String) =
      ArgsSummary(sstate, arguments.tail, errors :+ msg, warnings)

    def warn(msg: String) =
      ArgsSummary(sstate, arguments.tail, errors, warnings :+ msg)
  }

  case class Setting[T: ClassTag] private[Settings] (
    name: String,
    description: String,
    default: T,
    helpArg: String = "",
    choices: Seq[T] = Nil,
    prefix: String = "",
    aliases: List[String] = Nil,
    depends: List[(Setting[_], Any)] = Nil,
    propertyClass: Option[Class[_]] = None)(private[Settings] val idx: Int) {

    private var changed: Boolean = false

    def withAbbreviation(abbrv: String): Setting[T] =
      copy(aliases = aliases :+ abbrv)(idx)

    def dependsOn[U](setting: Setting[U], value: U): Setting[T] =
      copy(depends = depends :+ (setting, value))(idx)

    def valueIn(state: SettingsState): T =
      state.value(idx).asInstanceOf[T]

    def updateIn(state: SettingsState, x: Any): SettingsState = x match {
      case _: T => state.update(idx, x)
      case _ =>
        // would like to do:
        // throw new ClassCastException(s"illegal argument, found: $x of type ${x.getClass}, required: ${implicitly[ClassTag[T]]}")
        // but this runs afoul of primitive types. Concretely: if T is Boolean, then x is a boxed Boolean and the test will fail.
        // Maybe this is a bug in Scala 2.10?
        state.update(idx, x.asInstanceOf[T])
    }

    def isDefaultIn(state: SettingsState) = valueIn(state) == default

    def legalChoices: String =
      if (choices.isEmpty) ""
      else choices match {
        case r: Range => r.head + ".." + r.last
        case xs: List[_] => xs.mkString(", ")
      }

    def isLegal(arg: Any): Boolean =
      if (choices.isEmpty)
        arg match {
          case _: T => true
          case _ => false
        }
      else choices match {
        case r: Range =>
          arg match {
            case x: Int => r.head <= x && x <= r.last
            case _ => false
          }
        case xs: List[_] =>
          xs contains arg
      }

    def tryToSet(state: ArgsSummary): ArgsSummary = {
      val ArgsSummary(sstate, arg :: args, errors, warnings) = state
      def update(value: Any, args: List[String]) = {
        if (changed) {
          fail(s"Flag $name set repeatedly", args)
        } else {
          changed = true
          ArgsSummary(updateIn(sstate, value), args, errors, warnings)
        }
      }
      def fail(msg: String, args: List[String]) =
        ArgsSummary(sstate, args, errors :+ msg, warnings)
      def missingArg =
        fail(s"missing argument for option $name", args)
      def doSet(argRest: String) = ((implicitly[ClassTag[T]], args): @unchecked) match {
        case (BooleanTag, _) =>
          update(true, args)
        case (OptionTag, _) =>
          update(Some(propertyClass.get.newInstance), args)
        case (ListTag, _) =>
          if (argRest.isEmpty) missingArg
          else update((argRest split ",").toList, args)
        case (StringTag, _) if choices.nonEmpty =>
          if (argRest.isEmpty) missingArg
          else if (!choices.contains(argRest))
            fail(s"$arg is not a valid choice for $name", args)
          else update(argRest, args)
        case (StringTag, arg2 :: args2) =>
          update(arg2, args2)
        case (IntTag, arg2 :: args2) =>
          try {
            val x = arg2.toInt
            choices match {
              case r: Range if x < r.head || r.last < x =>
                fail(s"$arg2 is out of legal range $legalChoices for $name", args2)
              case _ =>
                update(x, args2)
            }
          } catch {
            case _: NumberFormatException =>
              fail(s"$arg2 is not an integer argument for $name", args2)
          }
        case (VersionTag, _) =>
          ScalaVersion.parse(argRest) match {
            case Success(v) => update(v, args)
            case Failure(ex) => fail(ex.getMessage, args)
          }
        case (_, Nil) =>
          missingArg
      }

      if (prefix != "" && arg.startsWith(prefix))
        doSet(arg drop prefix.length)
      else if (prefix == "" && name == arg.takeWhile(_ != ':'))
        doSet(arg.dropWhile(_ != ':').drop(1))
      else
        state
    }
  }

  object Setting {
    implicit class SettingDecorator[T](val setting: Setting[T]) extends AnyVal {
      def value(implicit ctx: Context): T = setting.valueIn(ctx.sstate)
      def update(x: T)(implicit ctx: Context): SettingsState = setting.updateIn(ctx.sstate, x)
      def isDefault(implicit ctx: Context): Boolean = setting.isDefaultIn(ctx.sstate)
    }
  }

  class SettingGroup {

    val _allSettings = new ArrayBuffer[Setting[_]]
    def allSettings: Seq[Setting[_]] = _allSettings

    def defaultState = new SettingsState(allSettings map (_.default))

    def userSetSettings(state: SettingsState) =
      allSettings filterNot (_.isDefaultIn(state))

    def toConciseString(state: SettingsState) =
      userSetSettings(state).mkString("(", " ", ")")

    private def checkDependencies(state: ArgsSummary): ArgsSummary =
      (state /: userSetSettings(state.sstate))(checkDependenciesOfSetting)

    private def checkDependenciesOfSetting(state: ArgsSummary, setting: Setting[_]) =
      (state /: setting.depends) { (s, dep) =>
        val (depSetting, reqValue) = dep
        if (depSetting.valueIn(state.sstate) == reqValue) s
        else s.fail(s"incomplete option ${setting.name} (requires ${depSetting.name})")
      }

    /** Iterates over the arguments applying them to settings where applicable.
     *  Then verifies setting dependencies are met.
     *
     *  This temporarily takes a boolean indicating whether to keep
     *  processing if an argument is seen which is not a command line option.
     *  This is an expedience for the moment so that you can say
     *
     *    scalac -d /tmp foo.scala -optimise
     *
     *  while also allowing
     *
     *    scala Program opt opt
     *
     *  to get their arguments.
     */
    protected def processArguments(state: ArgsSummary, processAll: Boolean, skipped: List[String]): ArgsSummary = {
      def stateWithArgs(args: List[String]) = ArgsSummary(state.sstate, args, state.errors, state.warnings)
      state.arguments match {
        case Nil =>
          checkDependencies(stateWithArgs(skipped))
        case "--" :: args =>
          checkDependencies(stateWithArgs(skipped ++ args))
        case x :: _ if x startsWith "-" =>
          @tailrec def loop(settings: List[Setting[_]]): ArgsSummary = settings match {
            case setting :: settings1 =>
              val state1 = setting.tryToSet(state)
              if (state1 ne state) processArguments(state1, processAll, skipped)
              else loop(settings1)
            case Nil =>
              processArguments(state.warn(s"bad option '$x' was ignored"), processAll, skipped)
          }
          loop(allSettings.toList)
        case arg :: args =>
          if (processAll) processArguments(stateWithArgs(args), processAll, skipped :+ arg)
          else state
      }
    }

    def processArguments(arguments: List[String], processAll: Boolean)(implicit ctx: Context): ArgsSummary =
      processArguments(ArgsSummary(ctx.sstate, arguments, Nil, Nil), processAll, Nil)

    def publish[T](settingf: Int => Setting[T]): Setting[T] = {
      val setting = settingf(_allSettings.length)
      _allSettings += setting
      setting
    }

    def BooleanSetting(name: String, descr: String, initialValue: Boolean = false): Setting[Boolean] =
      publish(Setting(name, descr, initialValue))

    def StringSetting(name: String, helpArg: String, descr: String, default: String): Setting[String] =
      publish(Setting(name, descr, default, helpArg))

    def ChoiceSetting(name: String, helpArg: String, descr: String, choices: List[String], default: String): Setting[String] =
      publish(Setting(name, descr, default, helpArg, choices))

    def IntSetting(name: String, descr: String, default: Int, range: Seq[Int] = Nil): Setting[Int] =
      publish(Setting(name, descr, default, choices = range))

    def MultiStringSetting(name: String, helpArg: String, descr: String): Setting[List[String]] =
      publish(Setting(name, descr, Nil, helpArg))

    def PathSetting(name: String, descr: String, default: String): Setting[String] =
      publish(Setting(name, descr, default))

    def PhasesSetting(name: String, descr: String, default: String = ""): Setting[List[String]] =
      publish(Setting(name, descr, if (default.isEmpty) Nil else List(default)))

    def PrefixSetting(name: String, pre: String, descr: String): Setting[List[String]] =
      publish(Setting(name, descr, Nil, prefix = pre))

    def VersionSetting(name: String, descr: String, default: ScalaVersion = NoScalaVersion): Setting[ScalaVersion] =
      publish(Setting(name, descr, default))

    def OptionSetting[T: ClassTag](name: String, descr: String): Setting[Option[T]] =
      publish(Setting(name, descr, None, propertyClass = Some(implicitly[ClassTag[T]].runtimeClass)))
  }
}
