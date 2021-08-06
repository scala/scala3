package dotty.tools.dotc
package config

import core.Contexts._

import dotty.tools.io.{AbstractFile, Directory, JarArchive, PlainDirectory}

import annotation.tailrec
import collection.mutable.ArrayBuffer
import language.existentials
import reflect.ClassTag
import scala.util.{Success, Failure}

object Settings:

  val BooleanTag: ClassTag[Boolean]      = ClassTag.Boolean
  val IntTag: ClassTag[Int]              = ClassTag.Int
  val StringTag: ClassTag[String]        = ClassTag(classOf[String])
  val ListTag: ClassTag[List[?]]         = ClassTag(classOf[List[?]])
  val VersionTag: ClassTag[ScalaVersion] = ClassTag(classOf[ScalaVersion])
  val OptionTag: ClassTag[Option[?]]     = ClassTag(classOf[Option[?]])
  val OutputTag: ClassTag[AbstractFile]  = ClassTag(classOf[AbstractFile])

  class SettingsState(initialValues: Seq[Any]):
    private val values = ArrayBuffer(initialValues: _*)
    private var _wasRead: Boolean = false

    override def toString: String = s"SettingsState(values: ${values.toList})"

    def value(idx: Int): Any =
      _wasRead = true
      values(idx)

    def update(idx: Int, x: Any): SettingsState =
      if (_wasRead) then SettingsState(values.toSeq).update(idx, x)
      else
        values(idx) = x
        this
  end SettingsState

  case class ArgsSummary(
    sstate: SettingsState,
    arguments: List[String],
    errors: List[String],
    warnings: List[String]) {

    def fail(msg: String): Settings.ArgsSummary =
      ArgsSummary(sstate, arguments.tail, errors :+ msg, warnings)

    def warn(msg: String): Settings.ArgsSummary =
      ArgsSummary(sstate, arguments.tail, errors, warnings :+ msg)
  }

  case class Setting[T: ClassTag] private[Settings] (
    name: String,
    description: String,
    default: T,
    helpArg: String = "",
    choices: Option[Seq[T]] = None,
    prefix: String = "",
    aliases: List[String] = Nil,
    depends: List[(Setting[?], Any)] = Nil,
    propertyClass: Option[Class[?]] = None)(private[Settings] val idx: Int) {

    private var changed: Boolean = false

    def valueIn(state: SettingsState): T = state.value(idx).asInstanceOf[T]

    def updateIn(state: SettingsState, x: Any): SettingsState = x match
      case _: T => state.update(idx, x)
      case _ => throw IllegalArgumentException(s"found: $x of type ${x.getClass.getName}, required: ${implicitly[ClassTag[T]]}")

    def isDefaultIn(state: SettingsState): Boolean = valueIn(state) == default

    def isMultivalue: Boolean = implicitly[ClassTag[T]] == ListTag

    def legalChoices: String =
      choices match {
        case Some(xs) if xs.isEmpty => ""
        case Some(r: Range)         => s"${r.head}..${r.last}"
        case Some(xs)               => xs.mkString(", ")
        case None                   => ""
      }

    def tryToSet(state: ArgsSummary): ArgsSummary = {
      val ArgsSummary(sstate, arg :: args, errors, warnings) = state
      def update(value: Any, args: List[String]): ArgsSummary =
        var dangers = warnings
        val value1 =
          if changed && isMultivalue then
            val value0  = value.asInstanceOf[List[String]]
            val current = valueIn(sstate).asInstanceOf[List[String]]
            value0.filter(current.contains).foreach(s => dangers :+= s"Setting $name set to $s redundantly")
            current ++ value0
          else
            if changed then dangers :+= s"Flag $name set repeatedly"
            value
        changed = true
        ArgsSummary(updateIn(sstate, value1), args, errors, dangers)
      end update
      def fail(msg: String, args: List[String]) =
        ArgsSummary(sstate, args, errors :+ msg, warnings)
      def missingArg =
        fail(s"missing argument for option $name", args)
      def setString(argValue: String, args: List[String]) =
        choices match
          case Some(xs) if !xs.contains(argValue) =>
            fail(s"$argValue is not a valid choice for $name", args)
          case _ =>
            update(argValue, args)
      def doSet(argRest: String) = ((implicitly[ClassTag[T]], args): @unchecked) match {
        case (BooleanTag, _) =>
          update(true, args)
        case (OptionTag, _) =>
          update(Some(propertyClass.get.getConstructor().newInstance()), args)
        case (ListTag, _) =>
          if (argRest.isEmpty) missingArg
          else update((argRest split ",").toList, args)
        case (StringTag, _) if argRest.nonEmpty || choices.exists(_.contains("")) =>
          setString(argRest, args)
        case (StringTag, arg2 :: args2) =>
          if (arg2 startsWith "-") missingArg
          else setString(arg2, args2)
        case (OutputTag, arg :: args) =>
          val path = Directory(arg)
          val isJar = path.extension == "jar"
          if (!isJar && !path.isDirectory)
            fail(s"'$arg' does not exist or is not a directory or .jar file", args)
          else {
            val output = if (isJar) JarArchive.create(path) else new PlainDirectory(path)
            update(output, args)
          }
        case (IntTag, _) =>
          val arg2 :: args2 = if (argRest == "") args else argRest :: args
          try {
            val x = arg2.toInt
            choices match {
              case Some(r: Range) if x < r.head || r.last < x =>
                fail(s"$arg2 is out of legal range ${r.head}..${r.last} for $name", args2)
              case Some(xs) if !xs.contains(x) =>
                fail(s"$arg2 is not a valid choice for $name", args)
              case _ =>
                update(x, args2)
            }
          }
          catch {
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

      def matches(argName: String) = (name :: aliases).exists(_ == argName)

      if (prefix != "" && arg.startsWith(prefix))
        doSet(arg drop prefix.length)
      else if (prefix == "" && matches(arg.takeWhile(_ != ':')))
        doSet(arg.dropWhile(_ != ':').drop(1))
      else
        state
    }
  }

  object Setting:
    extension [T](setting: Setting[T])
      def value(using Context): T = setting.valueIn(ctx.settingsState)
      def update(x: T)(using Context): SettingsState = setting.updateIn(ctx.settingsState, x)
      def isDefault(using Context): Boolean = setting.isDefaultIn(ctx.settingsState)

  class SettingGroup {

    private val _allSettings = new ArrayBuffer[Setting[?]]
    def allSettings: Seq[Setting[?]] = _allSettings.toSeq

    def defaultState: SettingsState = new SettingsState(allSettings map (_.default))

    def userSetSettings(state: SettingsState): Seq[Setting[?]] =
      allSettings filterNot (_.isDefaultIn(state))

    def toConciseString(state: SettingsState): String =
      userSetSettings(state).mkString("(", " ", ")")

    private def checkDependencies(state: ArgsSummary): ArgsSummary =
      userSetSettings(state.sstate).foldLeft(state)(checkDependenciesOfSetting)

    private def checkDependenciesOfSetting(state: ArgsSummary, setting: Setting[?]) =
      setting.depends.foldLeft(state) { (s, dep) =>
        val (depSetting, reqValue) = dep
        if (depSetting.valueIn(state.sstate) == reqValue) s
        else s.fail(s"incomplete option ${setting.name} (requires ${depSetting.name})")
      }

    /** Iterates over the arguments applying them to settings where applicable.
     *  Then verifies setting dependencies are met.
     *
     *  This takes a boolean indicating whether to keep
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
    @tailrec
    final def processArguments(state: ArgsSummary, processAll: Boolean, skipped: List[String]): ArgsSummary =
      def stateWithArgs(args: List[String]) = ArgsSummary(state.sstate, args, state.errors, state.warnings)
      state.arguments match
        case Nil =>
          checkDependencies(stateWithArgs(skipped))
        case "--" :: args =>
          checkDependencies(stateWithArgs(skipped ++ args))
        case x :: _ if x startsWith "-" =>
          @tailrec def loop(settings: List[Setting[?]]): ArgsSummary = settings match
            case setting :: settings1 =>
              val state1 = setting.tryToSet(state)
              if state1 ne state then state1
              else loop(settings1)
            case Nil =>
              state.warn(s"bad option '$x' was ignored")
          processArguments(loop(allSettings.toList), processAll, skipped)
        case arg :: args =>
          if processAll then processArguments(stateWithArgs(args), processAll, skipped :+ arg)
          else state
    end processArguments

    def processArguments(arguments: List[String], processAll: Boolean, settingsState: SettingsState = defaultState): ArgsSummary =
      processArguments(ArgsSummary(settingsState, arguments, Nil, Nil), processAll, Nil)

    def publish[T](settingf: Int => Setting[T]): Setting[T] = {
      val setting = settingf(_allSettings.length)
      _allSettings += setting
      setting
    }

    def BooleanSetting(name: String, descr: String, initialValue: Boolean = false, aliases: List[String] = Nil): Setting[Boolean] =
      publish(Setting(name, descr, initialValue, aliases = aliases))

    def StringSetting(name: String, helpArg: String, descr: String, default: String, aliases: List[String] = Nil): Setting[String] =
      publish(Setting(name, descr, default, helpArg, aliases = aliases))

    def ChoiceSetting(name: String, helpArg: String, descr: String, choices: List[String], default: String, aliases: List[String] = Nil): Setting[String] =
      publish(Setting(name, descr, default, helpArg, Some(choices), aliases = aliases))

    def IntSetting(name: String, descr: String, default: Int, aliases: List[String] = Nil): Setting[Int] =
      publish(Setting(name, descr, default, aliases = aliases))

    def IntChoiceSetting(name: String, descr: String, choices: Seq[Int], default: Int): Setting[Int] =
      publish(Setting(name, descr, default, choices = Some(choices)))

    def MultiStringSetting(name: String, helpArg: String, descr: String, aliases: List[String] = Nil): Setting[List[String]] =
      publish(Setting(name, descr, Nil, helpArg, aliases = aliases))

    def OutputSetting(name: String, helpArg: String, descr: String, default: AbstractFile): Setting[AbstractFile] =
      publish(Setting(name, descr, default, helpArg))

    def PathSetting(name: String, descr: String, default: String, aliases: List[String] = Nil): Setting[String] =
      publish(Setting(name, descr, default, aliases = aliases))

    def PhasesSetting(name: String, descr: String, default: String = "", aliases: List[String] = Nil): Setting[List[String]] =
      publish(Setting(name, descr, if (default.isEmpty) Nil else List(default), aliases = aliases))

    def PrefixSetting(name: String, pre: String, descr: String): Setting[List[String]] =
      publish(Setting(name, descr, Nil, prefix = pre))

    def VersionSetting(name: String, descr: String, default: ScalaVersion = NoScalaVersion): Setting[ScalaVersion] =
      publish(Setting(name, descr, default))

    def OptionSetting[T: ClassTag](name: String, descr: String, aliases: List[String] = Nil): Setting[Option[T]] =
      publish(Setting(name, descr, None, propertyClass = Some(implicitly[ClassTag[T]].runtimeClass), aliases = aliases))
  }
end Settings
