package dotty.tools.dotc
package config

import core.Contexts.*

import dotty.tools.io.{AbstractFile, Directory, JarArchive, PlainDirectory}

import annotation.tailrec
import annotation.internal.unshared
import collection.mutable, mutable.ArrayBuffer
import reflect.ClassTag
import scala.util.{Success, Failure}

import config.Settings.Setting.ChoiceWithHelp
import util.chaining.*

object Settings:

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

  case class ArgsSummary(sstate: SettingsState, arguments: List[String], errors: List[String], warnings: List[String])

  extension (summary: ArgsSummary)
    /** Error and update arguments */
    def fail(msg: String, arguments: List[String]): ArgsSummary =
      summary.copy(arguments = arguments, errors = summary.errors :+ msg)
    /** Error without updating arguments */
    def fail(msg: String): ArgsSummary =
      summary.copy(errors = summary.errors :+ msg)
    /** Warn and update arguments */
    def warn(msg: String, arguments: List[String]): ArgsSummary =
      summary.copy(arguments = arguments, warnings = summary.warnings :+ msg)
    /** Warn without updating arguments */
    def warn(msg: String): ArgsSummary =
      summary.copy(warnings = summary.warnings :+ msg)
    /** Update state and shift arguments */
    def updated(sstate: SettingsState, arguments: List[String]): ArgsSummary =
      summary.copy(sstate = sstate, arguments = arguments)
    /** Only shift arguments */
    def shifted(arguments: List[String]): ArgsSummary =
      summary.copy(arguments = arguments)
    /** Warn and skip, prepending the alternative arguments as substitution. */
    def deprecated(msg: String, altArgs: List[String], args: List[String]): ArgsSummary =
      summary.copy(arguments = altArgs ++ args, warnings = summary.warnings :+ msg)

  @unshared
  val settingCharacters = "[a-zA-Z0-9_\\-]*".r
  def validateSettingString(name: String): Unit =
    assert(settingCharacters.matches(name), s"Setting string $name contains invalid characters")

  val validTags = List(BooleanTag, IntTag, StringTag, ListTag, VersionTag, OptionTag, OutputTag)
  def validateSettingTag(ct: ClassTag[?]): Unit =
    assert(validTags.contains(ct), s"Unsupported option value $ct")

  /** List of setting-value pairs that are required for another setting to be valid.
    * For example, `s = Setting(..., depends = List(YprofileEnabled -> true))`
    * means that `s` requires `YprofileEnabled` to be set to `true`.
    */
  type SettingDependencies = List[(Setting[?], Any)]

  case class SettingAlias(name: String, deprecation: Option[Deprecation])
  object SettingAlias:
    given Conversion[String, SettingAlias] = SettingAlias(_, None)
    def apply(name: String): SettingAlias = SettingAlias(name, None)
    def apply(name: String, deprecation: Deprecation): SettingAlias = SettingAlias(name, Some(deprecation))

  case class Setting[T] private[Settings] (
    category: SettingCategory,
    name: String,
    description: String,
    default: T,
    helpArg: String = "",
    choices: Option[Seq[?]] = None,
    prefix: Option[String] = None,
    aliases: List[SettingAlias] = Nil,
    depends: SettingDependencies = Nil,
    ignoreInvalidArgs: Boolean = false,
    preferPrevious: Boolean = false,
    propertyClass: Option[Class[?]] = None,
    deprecation: Option[Deprecation] = None,
    // kept only for -Xkind-projector option compatibility
    legacyArgs: Boolean = false,
    // accept legacy choices (for example, valid in Scala 2 but no longer supported)
    legacyChoices: Option[Seq[?]] = None)(private[Settings] val idx: Int)(using ct: ClassTag[T]):

    validateSettingString(prefix.getOrElse(name))
    for alias <- aliases do
      validateSettingString(alias.name)
      val msg = "An alias is only \"replaced by\" its primary setting; its deprecation can have only a custom message."
      for dep <- alias.deprecation do
        assert(!dep.replacedBy.isDefined, msg)
    assert(name.startsWith(s"-${category.prefixLetter}"), s"Setting $name does not start with category -$category")
    assert(legacyArgs || !choices.exists(_.contains("")), s"Empty string is not supported as a choice for setting $name")
    validateSettingTag(ct)

    // Without the following assertion, it would be easy to mistakenly try to pass a file to a setting that ignores invalid args.
    // Example: -opt Main.scala would be interpreted as -opt:Main.scala, and the source file would be ignored.
    assert(!(ct == ListTag && ignoreInvalidArgs), s"Ignoring invalid args is not supported for multivalue settings: $name")

    val allFullNames: List[String] = s"$name" :: s"-$name" :: aliases.map(_.name)

    def valueIn(state: SettingsState): T = state.value(idx).asInstanceOf[T]

    def updateIn(state: SettingsState, x: Any): SettingsState = x match
      case _: T => state.update(idx, x)
      case _ => throw IllegalArgumentException(s"found: $x of type ${x.getClass.getName}, required: $ct")

    def isDefaultIn(state: SettingsState): Boolean = valueIn(state) == default

    def isMultivalue: Boolean = ct == ListTag

    def acceptsNoArg: Boolean = ct == BooleanTag || ct == OptionTag || choices.exists(_.contains(""))

    def legalChoices: String =
      choices match
        case Some(xs) if xs.isEmpty => ""
        case Some(r: Range)         => s"${r.head}..${r.last}"
        case Some(xs)               => xs.mkString(", ")
        case None                   => ""

    /** Updates the state from the next arg if this setting is applicable. */
    def tryToSet(state0: ArgsSummary): ArgsSummary =
      inline def state(using ArgsSummary) = summon[ArgsSummary]
      inline def sstate(using ArgsSummary) = state.sstate
      inline def changed(using ArgsSummary) = sstate.wasChanged(idx)
      val arg :: args = state0.arguments: @unchecked // there must be an arg

      /** Updates the value in state.
       *
       *  If this option is deprecated, add a warning.
       *  If the option is `replacedBy` another option, instead of applying the value,
       *  add the reconstructed arg to be applied subsequently.
       *
       *  @param value will be evaluated at most once for side effects
       *  @param altArg alt string to apply with alt setting if this setting is deprecated
       *  @param args remaining arguments to process
       *  @return updated argument state
       */
      def update(value: => Any, altArg: String, args: List[String])(using ArgsSummary): ArgsSummary =
        def checkDeprecatedAlias()(using ArgsSummary) =
          val name = arg.takeWhile(_ != ':')
          aliases.find(alt => alt.name == name && alt.deprecation.isDefined)
          .map:
            case SettingAlias(alt, dep) =>
              val msg =
                dep.collect:
                  case dep if dep.msg.nonEmpty => dep.msg
                .getOrElse(s"use ${this.name} instead")
              state.warn(s"Option $alt is a deprecated alias: $msg", args)
          .getOrElse(state)
        def doUpdate(using ArgsSummary) =
          deprecation match
          case Some(Deprecation(msg, Some(replacedBy))) =>
            val deprecatedMsg = s"Option $name is deprecated: $msg"
            val altArg1 =
              if altArg.isEmpty then List(replacedBy)
              else List(s"$replacedBy:$altArg")
            state.deprecated(deprecatedMsg, altArg1, args) // retry with reconstructed arg
          case Some(Deprecation(msg, _)) =>
            state.updated(updateIn(sstate, value), args) // allow but warn
              .warn(s"Option $name is deprecated: $msg")
          case None =>
            state.updated(updateIn(sstate, value), args)
        doUpdate(using checkDeprecatedAlias())
      end update

      def setBoolean(argValue: String, args: List[String])(using ArgsSummary) =
        def checkAndSet(v: Boolean) =
          val dubious = changed && v != valueIn(sstate).asInstanceOf[Boolean]
          if dubious then
            if preferPrevious then
              state.warn(s"Ignoring conflicting value for Boolean flag $name", args)
            else
              update(v, argValue, args).warn(s"Conflicting value for Boolean flag $name")
          else
            update(v, argValue, args)
        if argValue.isEmpty || argValue.equalsIgnoreCase("true") then checkAndSet(true)
        else if argValue.equalsIgnoreCase("false") then checkAndSet(false)
        else state.fail(s"$argValue is not a valid choice for Boolean flag $name", args)

      def setString(argValue: String, args: List[String])(using ArgsSummary) =
        choices match
        case Some(choices) if !choices.contains(argValue) =>
          state.fail(s"$argValue is not a valid choice for $name", args)
        case _ =>
          if changed && argValue != valueIn(sstate).asInstanceOf[String] then
            update(argValue, argValue, args).warn(s"Option $name was updated")
          else
            update(argValue, argValue, args)

      def setInt(argValue: String, args: List[String])(using ArgsSummary) =
        argValue.toIntOption.map: intValue =>
          choices match
          case Some(r: Range) if intValue < r.head || r.last < intValue =>
            state.fail(s"$argValue is out of legal range ${r.head}..${r.last} for $name", args)
          case Some(choices) if !choices.contains(intValue) =>
            state.fail(s"$argValue is not a valid choice for $name", args)
          case _ =>
            val dubious = changed && intValue != valueIn(sstate).asInstanceOf[Int]
            val updated = update(intValue, argValue, args)
            if dubious then updated.warn(s"Option $name was updated") else updated
        .getOrElse:
          state.fail(s"$argValue is not an integer argument for $name", args)

      def setOutput(arg: String, args: List[String])(using ArgsSummary) =
        val path = Directory(arg)
        val isJar = path.ext.isJar
        if (!isJar && !path.isDirectory) then
          state.fail(s"'$arg' does not exist or is not a directory or .jar file", args)
        else
          /* Side effect, do not change this method to evaluate eagerly */
          def output = if (isJar) JarArchive.create(path) else new PlainDirectory(path)
          val dubious = changed && output != valueIn(sstate).asInstanceOf[AbstractFile]
          val updated = update(output, arg, args)
          if dubious then updated.warn(s"Option $name was updated") else updated

      // argRest is the remainder of -foo:bar if any. This setting will receive a value from argRest or args.head.
      // useArg means use argRest even if empty.
      def doSet(argRest: String, useArg: Boolean)(using ArgsSummary): ArgsSummary =
        def missingArg =
          val msg = s"missing argument for option $name"
          if ignoreInvalidArgs then state.warn(s"$msg, the tag was ignored", args) else state.fail(msg, args)

        if ct == BooleanTag then setBoolean(argRest, args)
        else if ct == OptionTag then update(Some(propertyClass.get.getConstructor().newInstance()), "", args)
        else
          // `-option:v` or `-option v`
          val (arg1, args1) =
            val argInArgRest = useArg || !argRest.isEmpty || legacyArgs
            val useNextArg = !argInArgRest && args.nonEmpty && (ct == IntTag || !args.head.startsWith("-"))
            if argInArgRest then (argRest, args)
            else if useNextArg then (args.head, args.tail)
            else return missingArg
          def doSet(arg: String, args: List[String]) =
            ct match
            case _ if preferPrevious && changed =>
              if ignoreInvalidArgs then state.shifted(args)
              else state.warn(s"Ignoring update of option $name", args)
            case ListTag => setMultivalue(arg, args)
            case StringTag => setString(arg, args)
            case OutputTag => setOutput(arg, args)
            case IntTag => setInt(arg, args)
            case VersionTag => setVersion(arg, args)
            case _ => state.fail(s"unknown $ct", args)
          doSet(arg1, args1)
      end doSet

      def setVersion(arg: String, args: List[String])(using ArgsSummary) =
        ScalaVersion.parse(arg) match
        case Success(v) => update(v, arg, args)
        case Failure(e) => state.fail(e.getMessage, args)

      def setMultivalue(arg: String, args: List[String])(using ArgsSummary) =
        val split = arg.split(",").toList
        def setOrUpdate(actual: List[String]) =
          val updated =
            if changed then
              val current = valueIn(sstate).asInstanceOf[List[String]]
              current ++ actual
            else
              actual
          update(updated, arg, args)
        def invalidChoices(invalid: List[String]) =
          val msg = s"invalid choice(s) for $name: ${invalid.mkString(",")}"
          if ignoreInvalidArgs then state.warn(s"$msg, the tag was ignored", args) else state.fail(msg, args)

        choices match
        case Some(choices) =>
          split.partition(choices.contains) match
          case (_, Nil) => setOrUpdate(split)
          case (valid, invalid) =>
            legacyChoices match
            case Some(legacyChoices) =>
              invalid.filterNot(legacyChoices.contains) match
              case Nil => setOrUpdate(valid) // silently ignore legacy choices
              case invalid => invalidChoices(invalid)
            case none => invalidChoices(invalid)
        case none => setOrUpdate(split)
      end setMultivalue

      def matches: Boolean =
        val name = arg.takeWhile(_ != ':')
        allFullNames.exists(_ == name) || prefix.exists(arg.startsWith)

      if matches then
        given ArgsSummary = state0
        deprecation match
        case Some(Deprecation(msg, _)) if ignoreInvalidArgs => // a special case for Xlint
          state.warn(s"Option $name is deprecated: $msg", args)
        case _ =>
          prefix match
          case Some(prefix) =>
            val value = arg.drop(prefix.length)
            if value.isEmpty then
              state.warn(s"Option $name requires a suffix: $arg", args)
            else
              doSet(value, useArg = true)
          case none =>
            val split = arg.split(":", 2)
            if split.length == 1 then
              doSet("", useArg = false)
            else
              doSet(split(1), useArg = true)
      else state0

    end tryToSet
  end Setting

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
        case s: String => s == name.toString()
        case _ => false
      override def toString(): String =
        s"\n- $name${if description.isEmpty() then "" else s" :\n\t${description.replace("\n","\n\t")}"}"

    import ScalaSettingCategories.RootSetting
    def internal[T: ClassTag](name: String, value: T): Setting[T] =
      Setting(RootSetting, name, "internal", default = value)(-1)
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
    def apply(): Deprecation = Deprecation(msg = "", replacedBy = None)
    def renamed(replacement: String) = Some(Deprecation(s"Use $replacement instead.", Some(replacement)))
    def removed(removedVersion: Option[String] = None) =
      val msg = removedVersion.map(" in " + _).getOrElse(".")
      Some(Deprecation(s"Scheduled for removal$msg", None))

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
        if (depSetting.valueIn(s.sstate) == reqValue) s
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
    @tailrec
    final def processArguments(state: ArgsSummary, processAll: Boolean, skipped: List[String]): ArgsSummary =
      state.arguments match
        case Nil =>
          checkDependencies(state.shifted(skipped))
        case "--" :: args =>
          checkDependencies(state.shifted(skipped ++ args))
        case arg :: args if arg.startsWith("-") =>
          // find a setting to consume the next arg
          @tailrec def loop(settings: List[Setting[?]]): ArgsSummary = settings match
            case setting :: settings =>
              val state1 = setting.tryToSet(state)
              if state1 ne state then state1
              else loop(settings)
            case Nil =>
              state.warn(s"bad option '$arg' was ignored", args)
          processArguments(loop(allSettings.toList), processAll, skipped)
        case arg :: args =>
          if processAll then processArguments(state.shifted(args), processAll, skipped :+ arg)
          else state

    def processArguments(arguments: List[String], processAll: Boolean, settingsState: SettingsState = defaultState): ArgsSummary =
      processArguments(ArgsSummary(settingsState, arguments, errors = Nil, warnings = Nil), processAll, skipped = Nil)

    def publish[T](settingf: Int => Setting[T]): Setting[T] =
      val setting = settingf(_allSettings.length)
      _allSettings += setting
      setting

    def prependName(name: String): String =
      assert(!name.startsWith("-"), s"Setting $name cannot start with -")
      "-" + name

    def BooleanSetting(category: SettingCategory, name: String, descr: String, initialValue: Boolean = false, aliases: List[SettingAlias] = Nil, preferPrevious: Boolean = false, deprecation: Option[Deprecation] = None, ignoreInvalidArgs: Boolean = false): Setting[Boolean] =
      publish(Setting(category, prependName(name), descr, initialValue, aliases = aliases, preferPrevious = preferPrevious, deprecation = deprecation, ignoreInvalidArgs = ignoreInvalidArgs))

    def StringSetting(category: SettingCategory, name: String, helpArg: String, descr: String, default: String, aliases: List[SettingAlias] = Nil, deprecation: Option[Deprecation] = None, depends: SettingDependencies = Nil): Setting[String] =
      publish(Setting(category, prependName(name), descr, default, helpArg, aliases = aliases, deprecation = deprecation, depends = depends))

    def ChoiceSetting(category: SettingCategory, name: String, helpArg: String, descr: String, choices: List[String], default: String, aliases: List[SettingAlias] = Nil, legacyArgs: Boolean = false, deprecation: Option[Deprecation] = None): Setting[String] =
      publish(Setting(category, prependName(name), descr, default, helpArg, Some(choices), aliases = aliases, legacyArgs = legacyArgs, deprecation = deprecation))

    def MultiChoiceSetting(category: SettingCategory, name: String, helpArg: String, descr: String, choices: List[String], default: List[String] = Nil, legacyChoices: List[String] = Nil, aliases: List[SettingAlias] = Nil, deprecation: Option[Deprecation] = None): Setting[List[String]] =
      publish(Setting(category, prependName(name), descr, default, helpArg, Some(choices), legacyChoices = Some(legacyChoices), aliases = aliases, deprecation = deprecation))

    def MultiChoiceHelpSetting(category: SettingCategory, name: String, helpArg: String, descr: String, choices: List[ChoiceWithHelp[String]], default: List[ChoiceWithHelp[String]], legacyChoices: List[String] = Nil, aliases: List[SettingAlias] = Nil, deprecation: Option[Deprecation] = None): Setting[List[ChoiceWithHelp[String]]] =
      publish(Setting(category, prependName(name), descr, default, helpArg, Some(choices), legacyChoices = Some(legacyChoices), aliases = aliases, deprecation = deprecation))

    def IntSetting(category: SettingCategory, name: String, descr: String, default: Int, aliases: List[SettingAlias] = Nil, deprecation: Option[Deprecation] = None): Setting[Int] =
      publish(Setting(category, prependName(name), descr, default, aliases = aliases, deprecation = deprecation))

    def IntChoiceSetting(category: SettingCategory, name: String, descr: String, choices: Seq[Int], default: Int, deprecation: Option[Deprecation] = None): Setting[Int] =
      publish(Setting(category, prependName(name), descr, default, choices = Some(choices), deprecation = deprecation))

    def MultiStringSetting(category: SettingCategory, name: String, helpArg: String, descr: String, default: List[String] = Nil, aliases: List[SettingAlias] = Nil, deprecation: Option[Deprecation] = None): Setting[List[String]] =
      publish(Setting(category, prependName(name), descr, default, helpArg, aliases = aliases, deprecation = deprecation))

    def OutputSetting(category: SettingCategory, name: String, helpArg: String, descr: String, default: AbstractFile, aliases: List[SettingAlias] = Nil, preferPrevious: Boolean = false, deprecation: Option[Deprecation] = None, ignoreInvalidArgs: Boolean = false): Setting[AbstractFile] =
      publish(Setting(category, prependName(name), descr, default, helpArg, aliases = aliases, preferPrevious = preferPrevious, deprecation = deprecation, ignoreInvalidArgs = ignoreInvalidArgs))

    def PathSetting(category: SettingCategory, name: String, descr: String, default: String, aliases: List[SettingAlias] = Nil, deprecation: Option[Deprecation] = None): Setting[String] =
      publish(Setting(category, prependName(name), descr, default, aliases = aliases, deprecation = deprecation))

    def PhasesSetting(category: SettingCategory, name: String, descr: String, default: String = "", aliases: List[SettingAlias] = Nil, deprecation: Option[Deprecation] = None, depends: SettingDependencies = Nil): Setting[List[String]] =
      publish(Setting(category, prependName(name), descr, if (default.isEmpty) Nil else List(default), aliases = aliases, deprecation = deprecation, depends = depends))

    def PrefixSetting(category: SettingCategory, name0: String, descr: String, deprecation: Option[Deprecation] = None): Setting[List[String]] =
      val name = prependName(name0)
      val prefix = name.takeWhile(_ != '<')
      publish(Setting(category, name, descr, Nil, prefix = Some(prefix), deprecation = deprecation))

    def VersionSetting(category: SettingCategory, name: String, descr: String, default: ScalaVersion = NoScalaVersion, legacyArgs: Boolean = false, deprecation: Option[Deprecation] = None): Setting[ScalaVersion] =
      publish(Setting(category, prependName(name), descr, default, legacyArgs = legacyArgs, deprecation = deprecation))

    def OptionSetting[T: ClassTag](category: SettingCategory, name: String, descr: String, aliases: List[SettingAlias] = Nil, deprecation: Option[Deprecation] = None): Setting[Option[T]] =
      publish(Setting(category, prependName(name), descr, None, propertyClass = Some(summon[ClassTag[T]].runtimeClass), aliases = aliases, deprecation = deprecation))

  end SettingGroup
end Settings
