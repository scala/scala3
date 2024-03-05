package dotty.tools.dotc
package config

import scala.language.unsafeNulls

import core.Contexts.*

import dotty.tools.io.{AbstractFile, Directory, JarArchive, PlainDirectory}

import annotation.tailrec
import annotation.internal.unshared
import collection.mutable.ArrayBuffer
import collection.mutable
import reflect.ClassTag
import scala.util.{Success, Failure}
import dotty.tools.dotc.config.Settings.Setting.ChoiceWithHelp

object Settings:

  val BooleanTag: ClassTag[Boolean]      = ClassTag.Boolean
  val IntTag: ClassTag[Int]              = ClassTag.Int
  val StringTag: ClassTag[String]        = ClassTag(classOf[String])
  val ListTag: ClassTag[List[?]]         = ClassTag(classOf[List[?]])
  val VersionTag: ClassTag[ScalaVersion] = ClassTag(classOf[ScalaVersion])
  val OptionTag: ClassTag[Option[?]]     = ClassTag(classOf[Option[?]])
  val OutputTag: ClassTag[AbstractFile]  = ClassTag(classOf[AbstractFile])

  class SettingsState(initialValues: Seq[Any], initialChanged: Set[Int] = Set.empty):
    private val values = ArrayBuffer(initialValues*)
    private val changed: mutable.Set[Int] = initialChanged.to(mutable.Set)
    private var _wasRead: Boolean = false

    override def toString: String = s"SettingsState(values: ${values.toList})"

    def value(idx: Int): Any =
      _wasRead = true
      values(idx)

    def wasChanged(idx: Int): Boolean = changed.contains(idx)

    def update(idx: Int, x: Any): SettingsState =
      if (_wasRead) then SettingsState(values.toSeq, changed.toSet).update(idx, x)
      else
        values(idx) = x
        changed.add(idx)
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
    category: String,
    name: String,
    description: String,
    default: T,
    helpArg: String = "",
    choices: Option[Seq[?]] = None,
    prefix: Option[String] = None,
    aliases: List[String] = Nil,
    depends: List[(Setting[?], Any)] = Nil,
    ignoreInvalidArgs: Boolean = false,
    propertyClass: Option[Class[?]] = None,
    deprecationMsg: Option[String] = None)(private[Settings] val idx: Int) {
  
    
    assert(name.startsWith(s"-$category"), s"Setting $name does not start with category -$category")
    assert(!choices.contains(""), s"Empty string is not supported as a choice for setting $name")
    // Without the following assertion, it would be easy to mistakenly try to pass a file to a setting that ignores invalid args.
    // Example: -opt Main.scala would be interpreted as -opt:Main.scala, and the source file would be ignored.
    assert(!(summon[ClassTag[T]] == ListTag && ignoreInvalidArgs), s"Ignoring invalid args is not supported for multivalue settings: $name")

    val allFullNames: List[String] = s"$name" :: s"-$name" :: aliases  

    def valueIn(state: SettingsState): T = state.value(idx).asInstanceOf[T]

    def updateIn(state: SettingsState, x: Any): SettingsState = x match
      case _: T => state.update(idx, x)
      case _ => throw IllegalArgumentException(s"found: $x of type ${x.getClass.getName}, required: ${summon[ClassTag[T]]}")

    def isDefaultIn(state: SettingsState): Boolean = valueIn(state) == default

    def isMultivalue: Boolean = summon[ClassTag[T]] == ListTag

    def acceptsNoArg: Boolean = summon[ClassTag[T]] == BooleanTag || summon[ClassTag[T]] == OptionTag || choices.exists(_.contains(""))
    
    def legalChoices: String =
      choices match {
        case Some(xs) if xs.isEmpty => ""
        case Some(r: Range)         => s"${r.head}..${r.last}"
        case Some(xs)               => xs.mkString(", ")
        case None                   => ""
      }

    def tryToSet(state: ArgsSummary): ArgsSummary = {
      val ArgsSummary(sstate, arg :: args, errors, warnings) = state: @unchecked
      def update(value: Any, args: List[String]): ArgsSummary =
        var dangers = warnings
        val valueNew =
          if sstate.wasChanged(idx) && isMultivalue then
            val valueList = value.asInstanceOf[List[String]]
            val current = valueIn(sstate).asInstanceOf[List[String]]
            valueList.filter(current.contains).foreach(s => dangers :+= s"Setting $name set to $s redundantly")
            current ++ valueList
          else
            if sstate.wasChanged(idx) then dangers :+= s"Flag $name set repeatedly"
            value
        ArgsSummary(updateIn(sstate, valueNew), args, errors, dangers)
      end update

      def fail(msg: String, args: List[String]) =
        ArgsSummary(sstate, args, errors :+ msg, warnings)

      def warn(msg: String, args: List[String]) =
        ArgsSummary(sstate, args, errors, warnings :+ msg)

      def missingArg =
        val msg = s"missing argument for option $name"
        if ignoreInvalidArgs then warn(msg + ", the tag was ignored", args)  else fail(msg, args)

      def invalidChoices(invalid: List[String]) =
        val msg = s"invalid choice(s) for $name: ${invalid.mkString(",")}"
        if ignoreInvalidArgs then warn(msg + ", the tag was ignored", args) else fail(msg, args)

      def setBoolean(argValue: String, args: List[String]) =
        if argValue.equalsIgnoreCase("true") || argValue.isEmpty then update(true, args)
        else if argValue.equalsIgnoreCase("false") then update(false, args)
        else fail(s"$argValue is not a valid choice for boolean setting $name", args)

      def setString(argValue: String, args: List[String]) =
        choices match
          case Some(xs) if !xs.contains(argValue) =>
            fail(s"$argValue is not a valid choice for $name", args)
          case _ =>
            update(argValue, args)

      def setInt(argValue: String, args: List[String]) =
        try
          val x = argValue.toInt
          choices match
            case Some(r: Range) if x < r.head || r.last < x =>
              fail(s"$argValue is out of legal range ${r.head}..${r.last} for $name", args)
            case Some(xs) if !xs.contains(x) =>
              fail(s"$argValue is not a valid choice for $name", args)
            case _ =>
              update(x, args)
        catch case _: NumberFormatException =>
          fail(s"$argValue is not an integer argument for $name", args)
        
      def setOutput(argValue: String, args: List[String]) = 
        val path = Directory(argValue)
        val isJar = path.extension == "jar"
        if (!isJar && !path.isDirectory)
          fail(s"'$argValue' does not exist or is not a directory or .jar file", args)
        else {
          val output = if (isJar) JarArchive.create(path) else new PlainDirectory(path)
          update(output, args)
        }
      
      def setVersion(argValue: String, args: List[String]) =
        ScalaVersion.parse(argValue) match {
          case Success(v) => update(v, args)
          case Failure(ex) => fail(ex.getMessage, args)
        }

      def appendList(strings: List[String], args: List[String]) =
        choices match
          case Some(valid) => strings.filterNot(valid.contains) match
            case Nil => update(strings, args)
            case invalid => invalidChoices(invalid)
          case _ => update(strings, args)


      def doSet(argRest: String) = 
        ((summon[ClassTag[T]], args): @unchecked) match {
          case (BooleanTag, _) =>
            setBoolean(argRest, args)
          case (OptionTag, _) =>
            update(Some(propertyClass.get.getConstructor().newInstance()), args)
          case (_, args) =>
            val argInArgRest = !argRest.isEmpty
            val argAfterParam = !argInArgRest && args.nonEmpty && !args.head.startsWith("-")
            if argInArgRest then
              doSetArg(argRest, args)
            else if argAfterParam then
              doSetArg(args.head, args.tail)
            else missingArg
        }

      def doSetArg(arg: String, argsLeft: List[String]) = summon[ClassTag[T]] match
          case ListTag =>
            val strings = arg.split(",").toList
            appendList(strings, argsLeft)
          case StringTag =>
            setString(arg, argsLeft)
          case OutputTag =>
            setOutput(arg, argsLeft)
          case IntTag =>
            setInt(arg, argsLeft)
          case VersionTag =>
            setVersion(arg, argsLeft)
          case _ =>
            missingArg

      def matches(argName: String): Boolean = 
        (allFullNames).exists(_ == argName.takeWhile(_ != ':')) || prefix.exists(arg.startsWith)

      def argValRest: String = 
        if(prefix.isEmpty) arg.dropWhile(_ != ':').drop(1) else arg.drop(prefix.get.length)
      
      if matches(arg) then 
        if deprecationMsg.isDefined then
          warn(s"Option $name is deprecated: ${deprecationMsg.get}", args)
        else 
          doSet(argValRest)
      else
        state
    }
  }

  object Setting:
    extension [T](setting: Setting[T])
      def value(using Context): T = setting.valueIn(ctx.settingsState)
      def update(x: T)(using Context): SettingsState = setting.updateIn(ctx.settingsState, x)
      def isDefault(using Context): Boolean = setting.isDefaultIn(ctx.settingsState)

    /**
     * A choice with help description.
     *
     * NOTE : `equals` and `toString` have special behaviors
     */
    case class ChoiceWithHelp[T](name: T, description: String):
      override def equals(x: Any): Boolean = x match
        case s:String => s == name.toString()
        case _ => false
      override def toString(): String =
        s"\n- $name${if description.isEmpty() then "" else s" :\n\t${description.replace("\n","\n\t")}"}"
  end Setting

  class SettingGroup {

    @unshared
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

    @unshared
    val settingCharacters = "[a-zA-Z0-9_\\-]*".r
    def validateSetting(setting: String): String =
      assert(settingCharacters.matches(setting), s"Setting $setting contains invalid characters")
      setting 

    def validateAndPrependName(name: String): String =
      assert(!name.startsWith("-"), s"Setting $name cannot start with -")
      "-" + validateSetting(name)

    def BooleanSetting(category: String, name: String, descr: String, initialValue: Boolean = false, aliases: List[String] = Nil): Setting[Boolean] =
      publish(Setting(category, validateAndPrependName(name), descr, initialValue, aliases = aliases.map(validateSetting)))

    def StringSetting(category: String, name: String, helpArg: String, descr: String, default: String, aliases: List[String] = Nil): Setting[String] =
      publish(Setting(category, validateAndPrependName(name), descr, default, helpArg, aliases = aliases.map(validateSetting)))

    def ChoiceSetting(category: String, name: String, helpArg: String, descr: String, choices: List[String], default: String, aliases: List[String] = Nil): Setting[String] =
      publish(Setting(category, validateAndPrependName(name), descr, default, helpArg, Some(choices), aliases = aliases.map(validateSetting)))

    def MultiChoiceSetting(category: String, name: String, helpArg: String, descr: String, choices: List[String], default: List[String], aliases: List[String] = Nil): Setting[List[String]] =
      publish(Setting(category, validateAndPrependName(name), descr, default, helpArg, Some(choices), aliases = aliases.map(validateSetting)))

    def MultiChoiceHelpSetting(category: String, name: String, helpArg: String, descr: String, choices: List[ChoiceWithHelp[String]], default: List[ChoiceWithHelp[String]], aliases: List[String] = Nil): Setting[List[ChoiceWithHelp[String]]] =
      publish(Setting(category, validateAndPrependName(name), descr, default, helpArg, Some(choices), aliases = aliases.map(validateSetting)))

    def IntSetting(category: String, name: String, descr: String, default: Int, aliases: List[String] = Nil): Setting[Int] =
      publish(Setting(category, validateAndPrependName(name), descr, default, aliases = aliases.map(validateSetting)))

    def IntChoiceSetting(category: String, name: String, descr: String, choices: Seq[Int], default: Int): Setting[Int] =
      publish(Setting(category, validateAndPrependName(name), descr, default, choices = Some(choices)))

    def MultiStringSetting(category: String, name: String, helpArg: String, descr: String, default: List[String] = Nil, aliases: List[String] = Nil): Setting[List[String]] =
      publish(Setting(category, validateAndPrependName(name), descr, default, helpArg, aliases = aliases.map(validateSetting)))

    def OutputSetting(category: String, name: String, helpArg: String, descr: String, default: AbstractFile): Setting[AbstractFile] =
      publish(Setting(category, validateAndPrependName(name), descr, default, helpArg))

    def PathSetting(category: String, name: String, descr: String, default: String, aliases: List[String] = Nil): Setting[String] =
      publish(Setting(category, validateAndPrependName(name), descr, default, aliases = aliases.map(validateSetting)))

    def PhasesSetting(category: String, name: String, descr: String, default: String = "", aliases: List[String] = Nil): Setting[List[String]] =
      publish(Setting(category, validateAndPrependName(name), descr, if (default.isEmpty) Nil else List(default), aliases = aliases.map(validateSetting)))

    def PrefixSetting(category: String, name: String, descr: String): Setting[List[String]] =
      val prefix = name.takeWhile(_ != '<')
      publish(Setting(category, "-" + name, descr, Nil, prefix = Some(validateSetting(prefix))))

    def VersionSetting(category: String, name: String, descr: String, default: ScalaVersion = NoScalaVersion): Setting[ScalaVersion] =
      publish(Setting(category, validateAndPrependName(name), descr, default))

    def OptionSetting[T: ClassTag](category: String, name: String, descr: String, aliases: List[String] = Nil): Setting[Option[T]] =
      publish(Setting(category, validateAndPrependName(name), descr, None, propertyClass = Some(summon[ClassTag[T]].runtimeClass), aliases = aliases.map(validateSetting)))
    
    def DeprecatedSetting(category: String, name: String, descr: String, deprecationMsg: String): Setting[Boolean] =
      publish(Setting(category, validateAndPrependName(name), descr, false, deprecationMsg = Some(deprecationMsg)))
  }
end Settings
