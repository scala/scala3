package dotty.tools.dotc
package config

import core.Contexts.*

import dotty.tools.io.{AbstractFile, Directory, JarArchive, PlainDirectory}

import annotation.internal.unshared
import collection.mutable.ArrayBuffer
import collection.mutable
import reflect.ClassTag
import scala.util.{Success, Failure}
import dotty.tools.dotc.config.Settings.Setting.ChoiceWithHelp

object Settings:

  private inline def classTag[T](using ctag: ClassTag[T]): ClassTag[T] = ctag

  val BooleanTag: ClassTag[Boolean]      = ClassTag.Boolean
  val IntTag: ClassTag[Int]              = ClassTag.Int
  val StringTag: ClassTag[String]        = ClassTag(classOf[String])
  val ListTag: ClassTag[List[?]]         = ClassTag(classOf[List[?]])
  val VersionTag: ClassTag[ScalaVersion] = ClassTag(classOf[ScalaVersion])
  val OptionTag: ClassTag[Option[?]]     = ClassTag(classOf[Option[?]])
  val OutputTag: ClassTag[AbstractFile]  = ClassTag(classOf[AbstractFile])

  trait SettingCategory:
    def prefixLetter: String

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

    def reinitializedCopy(): SettingsState =
      SettingsState(values.toSeq, changed.toSet)

  end SettingsState

  case class ArgsSummary(
    sstate: SettingsState,
    arguments: List[String],
    errors: List[String],
    warnings: List[String]):

    def fail(msg: String): Settings.ArgsSummary =
      ArgsSummary(sstate, arguments.tail, errors :+ msg, warnings)

    def warn(msg: String): Settings.ArgsSummary =
      ArgsSummary(sstate, arguments.tail, errors, warnings :+ msg)

    def deprecated(msg: String, extraArgs: List[String] = Nil): Settings.ArgsSummary =
      ArgsSummary(sstate, extraArgs ++ arguments.tail, errors, warnings :+ msg)

  @unshared
  val settingCharacters = "[a-zA-Z0-9_\\-]*".r
  def validateSettingString(name: String): Unit =
    assert(settingCharacters.matches(name), s"Setting string $name contains invalid characters")

  /** List of setting-value pairs that are required for another setting to be valid.
    * For example, `s = Setting(..., depends = List(YprofileEnabled -> true))`
    * means that `s` requires `YprofileEnabled` to be set to `true`.
    */
  type SettingDependencies = List[(Setting[?], Any)]

  case class Setting[T: ClassTag] private[Settings] (
    category: SettingCategory,
    name: String,
    description: String,
    default: T,
    helpArg: String = "",
    choices: Option[Seq[?]] = None,
    prefix: Option[String] = None,
    aliases: List[String] = Nil,
    depends: SettingDependencies = Nil,
    ignoreInvalidArgs: Boolean = false,
    preferPrevious: Boolean = false,
    propertyClass: Option[Class[?]] = None,
    deprecation: Option[Deprecation] = None,
    // kept only for -Xkind-projector option compatibility
    legacyArgs: Boolean = false,
    // accept legacy choices (for example, valid in Scala 2 but no longer supported)
    legacyChoices: Option[Seq[?]] = None)(private[Settings] val idx: Int):

    validateSettingString(prefix.getOrElse(name))
    aliases.foreach(validateSettingString)
    assert(name.startsWith(s"-${category.prefixLetter}"), s"Setting $name does not start with category -$category")
    assert(legacyArgs || !choices.exists(_.contains("")), s"Empty string is not supported as a choice for setting $name")
    // Without the following assertion, it would be easy to mistakenly try to pass a file to a setting that ignores invalid args.
    // Example: -opt Main.scala would be interpreted as -opt:Main.scala, and the source file would be ignored.
    assert(!(classTag[T] == ListTag && ignoreInvalidArgs), s"Ignoring invalid args is not supported for multivalue settings: $name")

    val allFullNames: List[String] = s"$name" :: s"-$name" :: aliases

    def isPresentIn(state: SettingsState): Boolean = state.wasChanged(idx)

    def valueIn(state: SettingsState): T = state.value(idx).asInstanceOf[T]

    def userValueIn(state: SettingsState): Option[T] = if isPresentIn(state) then Some(valueIn(state)) else None

    def updateIn(state: SettingsState, x: Any): SettingsState = x match
      case _: T => state.update(idx, x)
      case null => throw IllegalArgumentException(s"attempt to set null ${classTag[T]}")
      case _    => throw IllegalArgumentException(s"found: $x of type ${x.getClass.getName}, required: ${classTag[T]}")

    def isDefaultIn(state: SettingsState): Boolean = valueIn(state) == default

    def isMultivalue: Boolean = classTag[T] == ListTag

    def legalChoices: String =
      choices match
        case Some(xs) if xs.isEmpty => ""
        case Some(r: Range)         => s"${r.head}..${r.last}"
        case Some(xs)               => xs.mkString(", ")
        case None                   => ""

    def tryToSet(state: ArgsSummary): ArgsSummary =
      val ArgsSummary(sstate, arg :: args, errors, warnings) = state: @unchecked

      /**
        * Updates the value in state
        *
        * @param getValue it is crucial that this argument is passed by name, as [setOutput] have side effects.
        * @param argStringValue string value of currently processed argument that will be used to set deprecation replacement
        * @param args remaining arguments to process
        * @return new argumment state
        */
      def update(getValue: => Any, argStringValue: String, args: List[String]): ArgsSummary =
        deprecation match
          case Some(Deprecation(msg, Some(replacedBy))) =>
            val deprecatedMsg = s"Option $name is deprecated: $msg"
            if argStringValue.isEmpty then state.deprecated(deprecatedMsg, List(replacedBy))
            else state.deprecated(deprecatedMsg, List(s"$replacedBy:$argStringValue"))

          case Some(Deprecation(msg, _)) =>
            state.deprecated(s"Option $name is deprecated: $msg")

          case None =>
            val value = getValue
            var dangers = warnings
            val valueNew =
              if sstate.wasChanged(idx) && isMultivalue then
                val valueList = value.asInstanceOf[List[String]]
                val current = valueIn(sstate).asInstanceOf[List[String]]
                valueList.filter(current.contains).foreach(s => dangers :+= s"Setting $name set to $s redundantly")
                current ++ valueList
              else
                if sstate.wasChanged(idx) then
                  assert(!preferPrevious, "should have shortcutted with ignoreValue, side-effect may be present!")
                  dangers :+= s"Flag $name set repeatedly"
                value
            ArgsSummary(updateIn(sstate, valueNew), args, errors, dangers)
      end update

      def ignoreValue(args: List[String]): ArgsSummary =
        ArgsSummary(sstate, args, errors, warnings)

      def missingArg =
        val msg = s"missing argument for option $name"
        if ignoreInvalidArgs then state.warn(s"$msg, the tag was ignored") else state.fail(msg)

      def invalidChoices(invalid: List[String]) =
        val msg = s"invalid choice(s) for $name: ${invalid.mkString(",")}"
        if ignoreInvalidArgs then state.warn(s"$msg, the tag was ignored") else state.fail(msg)

      def isEmptyDefault = default == null.asInstanceOf[T] || classTag[T].match
        case ListTag   => default.asInstanceOf[List[?]].isEmpty
        case StringTag => default.asInstanceOf[String].isEmpty
        case OptionTag => default.asInstanceOf[Option[?]].isEmpty
        case _         => false

      def setBoolean(argValue: String, args: List[String]) =
        if argValue.equalsIgnoreCase("true") || argValue.isEmpty then update(true, argValue, args)
        else if argValue.equalsIgnoreCase("false") then update(false, argValue, args)
        else state.fail(s"$argValue is not a valid choice for boolean setting $name")

      def setString(argValue: String, args: List[String]) =
        choices match
          case Some(xs) if !xs.contains(argValue) =>
            state.fail(s"$argValue is not a valid choice for $name")
          case _ =>
            update(argValue, argValue, args)

      def setInt(argValue: String, args: List[String]) =
        argValue.toIntOption.map: intValue =>
          choices match
            case Some(r: Range) if intValue < r.head || r.last < intValue =>
              state.fail(s"$argValue is out of legal range ${r.head}..${r.last} for $name")
            case Some(xs) if !xs.contains(intValue) =>
              state.fail(s"$argValue is not a valid choice for $name")
            case _ =>
              update(intValue, argValue, args)
        .getOrElse:
          state.fail(s"$argValue is not an integer argument for $name")

      def setOutput(argValue: String, args: List[String]) =
        val path = Directory(argValue)
        val isJar = path.ext.isJar
        if !isJar && !path.isDirectory then
          state.fail(s"'$argValue' does not exist or is not a directory or .jar file")
        else
          /* Side effect, do not change this method to evaluate eagerly */
          def output = if isJar then JarArchive.create(path) else new PlainDirectory(path)
          update(output, argValue, args)

      def setVersion(argValue: String, args: List[String]) =
        ScalaVersion.parse(argValue) match
          case Success(v) => update(v, argValue, args)
          case Failure(ex) => state.fail(ex.getMessage)

      def appendList(strings: List[String], argValue: String, args: List[String]) =
        choices match
          case Some(valid) => strings.partition(valid.contains) match
            case (_, Nil) => update(strings, argValue, args)
            case (validStrs, invalidStrs) => legacyChoices match
              case Some(validBefore) =>
                invalidStrs.filterNot(validBefore.contains) match
                  case Nil => update(validStrs, argValue, args)
                  case realInvalidStrs => invalidChoices(realInvalidStrs)
              case _ => invalidChoices(invalidStrs)
          case _ => update(strings, argValue, args)

      def doSet(argRest: String) =
        classTag[T] match
          case BooleanTag =>
            if sstate.wasChanged(idx) && preferPrevious then ignoreValue(args)
            else setBoolean(argRest, args)
          case OptionTag =>
            update(Some(propertyClass.get.getConstructor().newInstance()), "", args)
          case ct =>
            val argInArgRest = !argRest.isEmpty || legacyArgs
            inline def argAfterParam = args.nonEmpty && (ct == IntTag || !args.head.startsWith("-"))
            inline def isMultivalueWithDefault = isMultivalue && !isEmptyDefault
            if argInArgRest then
              doSetArg(argRest, args)
            else if argAfterParam && !isMultivalueWithDefault then
              doSetArg(args.head, args.tail)
            else if isEmptyDefault then
              missingArg
            else
              doSetArg(arg = null, args) // update with default

      def doSetArg(arg: String | Null, argsLeft: List[String]) =
        arg match
        case null =>
          classTag[T] match
          case ListTag =>
            update(default, argStringValue = "", argsLeft)
          case _ =>
            missingArg
        case arg =>
          classTag[T] match
          case ListTag =>
            appendList(arg.split(",").toList, arg, argsLeft)
          case StringTag =>
            setString(arg, argsLeft)
          case OutputTag =>
            if sstate.wasChanged(idx) && preferPrevious then
              ignoreValue(argsLeft) // do not risk side effects e.g. overwriting a jar
            else
              setOutput(arg, argsLeft)
          case IntTag =>
            setInt(arg, argsLeft)
          case VersionTag =>
            setVersion(arg, argsLeft)
          case _ =>
            missingArg

      def matches(argName: String): Boolean =
        allFullNames.exists(_ == argName.takeWhile(_ != ':')) || prefix.exists(arg.startsWith)

      def argValRest: String =
        if prefix.isEmpty then arg.dropWhile(_ != ':').drop(1) else arg.drop(prefix.get.length)

      if matches(arg) then
        deprecation match
          case Some(Deprecation(msg, _)) if ignoreInvalidArgs => // a special case for Xlint
            state.deprecated(s"Option $name is deprecated: $msg")
          case _ => doSet(argValRest)
      else state

    end tryToSet
  end Setting

  /**
    * Class used for deprecating purposes.
    * It contains all necessary information to deprecate given option.
    * Scala Settings are considered deprecated when this object is present at their creation site.
    *
    * @param msg           deprecation message that will be displayed in following format: s"Option $name is deprecated: $msg"
    * @param replacedBy    option that is substituting current option
    */
  case class Deprecation(
    msg: String,
    replacedBy: Option[String] = None,
  )

  object Deprecation:
    def renamed(replacement: String) = Some(Deprecation(s"Use $replacement instead.", Some(replacement)))
    def removed(removedVersion: Option[String] = None) =
      val msg = removedVersion.map(" in " + _).getOrElse(".")
      Some(Deprecation(s"Scheduled for removal$msg", None))

  object Setting:
    extension [T](setting: Setting[T])
      def value(using Context): T = setting.valueIn(ctx.settingsState)
      def userValue(using Context): Option[T] = setting.userValueIn(ctx.settingsState)
      def update(x: T)(using Context): SettingsState = setting.updateIn(ctx.settingsState, x)
      def isDefault(using Context): Boolean = setting.isDefaultIn(ctx.settingsState)
      def isPresent(using Context): Boolean = setting.isPresentIn(ctx.settingsState)

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

  class SettingGroup:

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
      setting.depends.foldLeft(state): (s, dep) =>
        val (depSetting, reqValue) = dep
        if (depSetting.valueIn(state.sstate) == reqValue) s
        else s.fail(s"incomplete option ${setting.name} (requires ${depSetting.name})")

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
    final def processArguments(state: ArgsSummary, processAll: Boolean, skipped: List[String]): ArgsSummary =
      def stateWithArgs(args: List[String]) = ArgsSummary(state.sstate, args, state.errors, state.warnings)
      state.arguments match
        case Nil =>
          checkDependencies(stateWithArgs(skipped))
        case "--" :: args =>
          checkDependencies(stateWithArgs(skipped ++ args))
        case x :: _ if x startsWith "-" =>
          def loop(settings: List[Setting[?]]): ArgsSummary = settings match
            case setting :: settings =>
              val state1 = setting.tryToSet(state)
              if state1 ne state then state1
              else loop(settings)
            case Nil =>
              state.warn(s"bad option '$x' was ignored")
          processArguments(loop(allSettings.toList), processAll, skipped)
        case arg :: args =>
          if processAll then processArguments(stateWithArgs(args), processAll, skipped :+ arg)
          else state
    end processArguments

    def processArguments(arguments: List[String], processAll: Boolean, settingsState: SettingsState = defaultState): ArgsSummary =
      processArguments(ArgsSummary(settingsState, arguments, errors = Nil, warnings = Nil), processAll, skipped = Nil)

    def publish[T](settingf: Int => Setting[T]): Setting[T] =
      val setting = settingf(_allSettings.length)
      _allSettings += setting
      setting

    def prependName(name: String): String =
      assert(!name.startsWith("-"), s"Setting $name cannot start with -")
      "-" + name

    def BooleanSetting(category: SettingCategory, name: String, descr: String, initialValue: Boolean = false, aliases: List[String] = Nil, preferPrevious: Boolean = false, deprecation: Option[Deprecation] = None, ignoreInvalidArgs: Boolean = false): Setting[Boolean] =
      publish(Setting(category, prependName(name), descr, initialValue, aliases = aliases, preferPrevious = preferPrevious, deprecation = deprecation, ignoreInvalidArgs = ignoreInvalidArgs))

    def StringSetting(category: SettingCategory, name: String, helpArg: String, descr: String, default: String, aliases: List[String] = Nil, deprecation: Option[Deprecation] = None, depends: SettingDependencies = Nil): Setting[String] =
      publish(Setting(category, prependName(name), descr, default, helpArg, aliases = aliases, deprecation = deprecation, depends = depends))

    def ChoiceSetting(category: SettingCategory, name: String, helpArg: String, descr: String, choices: List[String], default: String, aliases: List[String] = Nil, legacyArgs: Boolean = false, deprecation: Option[Deprecation] = None): Setting[String] =
      publish(Setting(category, prependName(name), descr, default, helpArg, Some(choices), aliases = aliases, legacyArgs = legacyArgs, deprecation = deprecation))

    def MultiChoiceSetting(category: SettingCategory, name: String, helpArg: String, descr: String, choices: List[String], default: List[String] = Nil, legacyChoices: List[String] = Nil, aliases: List[String] = Nil, deprecation: Option[Deprecation] = None): Setting[List[String]] =
      publish(Setting(category, prependName(name), descr, default, helpArg, Some(choices), legacyChoices = Some(legacyChoices), aliases = aliases, deprecation = deprecation))

    def MultiChoiceHelpSetting(category: SettingCategory, name: String, helpArg: String, descr: String, choices: List[ChoiceWithHelp[String]], default: List[ChoiceWithHelp[String]] = Nil, legacyChoices: List[String] = Nil, aliases: List[String] = Nil, deprecation: Option[Deprecation] = None): Setting[List[ChoiceWithHelp[String]]] =
      publish(Setting(category, prependName(name), descr, default, helpArg, Some(choices), legacyChoices = Some(legacyChoices), aliases = aliases, deprecation = deprecation))

    def IntSetting(category: SettingCategory, name: String, descr: String, default: Int, aliases: List[String] = Nil, deprecation: Option[Deprecation] = None): Setting[Int] =
      publish(Setting(category, prependName(name), descr, default, aliases = aliases, deprecation = deprecation))

    def IntChoiceSetting(category: SettingCategory, name: String, descr: String, choices: Seq[Int], default: Int, deprecation: Option[Deprecation] = None): Setting[Int] =
      publish(Setting(category, prependName(name), descr, default, choices = Some(choices), deprecation = deprecation))

    def MultiStringSetting(category: SettingCategory, name: String, helpArg: String, descr: String, default: List[String] = Nil, aliases: List[String] = Nil, deprecation: Option[Deprecation] = None): Setting[List[String]] =
      publish(Setting(category, prependName(name), descr, default, helpArg, aliases = aliases, deprecation = deprecation))

    def OutputSetting(category: SettingCategory, name: String, helpArg: String, descr: String, default: AbstractFile, aliases: List[String] = Nil, preferPrevious: Boolean = false, deprecation: Option[Deprecation] = None): Setting[AbstractFile] =
      publish(Setting(category, prependName(name), descr, default, helpArg, aliases = aliases, preferPrevious = preferPrevious, deprecation = deprecation))

    def PathSetting(category: SettingCategory, name: String, descr: String, default: String, aliases: List[String] = Nil, deprecation: Option[Deprecation] = None): Setting[String] =
      publish(Setting(category, prependName(name), descr, default, aliases = aliases, deprecation = deprecation))

    def PhasesSetting(category: SettingCategory, name: String, descr: String, default: String = "", aliases: List[String] = Nil, deprecation: Option[Deprecation] = None, depends: SettingDependencies = Nil): Setting[List[String]] =
      publish(Setting(category, prependName(name), descr, if (default.isEmpty) Nil else List(default), aliases = aliases, deprecation = deprecation, depends = depends))

    def PrefixSetting(category: SettingCategory, name0: String, descr: String, deprecation: Option[Deprecation] = None): Setting[List[String]] =
      val name = prependName(name0)
      val prefix = name.takeWhile(_ != '<')
      publish(Setting(category, name, descr, Nil, prefix = Some(prefix), deprecation = deprecation))

    def VersionSetting(category: SettingCategory, name: String, descr: String, default: ScalaVersion = NoScalaVersion, legacyArgs: Boolean = false, deprecation: Option[Deprecation] = None): Setting[ScalaVersion] =
      publish(Setting(category, prependName(name), descr, default, legacyArgs = legacyArgs, deprecation = deprecation))

    def OptionSetting[T: ClassTag](category: SettingCategory, name: String, descr: String, aliases: List[String] = Nil, deprecation: Option[Deprecation] = None): Setting[Option[T]] =
      publish(Setting(category, prependName(name), descr, None, propertyClass = Some(classTag[T].runtimeClass), aliases = aliases, deprecation = deprecation))

  end SettingGroup
end Settings
