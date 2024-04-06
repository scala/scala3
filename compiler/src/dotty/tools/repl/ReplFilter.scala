package dotty.tools.repl

import dotty.tools.dotc.core.Contexts.*

import java.io.PrintStream
import java.lang.reflect.Constructor
import scala.util.{Failure, Success, Try}


trait ReplFilter

object ReplFilter:
  private var replFilterOpt: Option[ReplFilter] = None
  private var isInitialized_ : Boolean = false
  def init(rootCtx: Context): Either[Throwable, ReplFilter] =
    val returnValue =
      rootCtx.settings.rootSettings.find(_.name == "-replfilter") match
        case Some(replFilterSetting) =>
          val replFilterClassName = inContext[String](rootCtx){replFilterSetting.value.asInstanceOf[String]}
          Try(Class.forName(replFilterClassName)) match
            case Success(clazz : Class[?]) =>
              clazz.getDeclaredConstructor() match
                case c: Constructor[?] => 
                  c.newInstance() match
                    case rf: ReplFilter => 
                      replFilterOpt = Some(rf)
                      Right(rf)
                    case rf if rf != null => Left(RuntimeException(s"Provided REPL filter class has an inappropriate type"))
                    case _ => Left(RuntimeException(s"Provided REPL filter class can't be instantiated"))
                case _ => Left(RuntimeException(s"Provided REPL filter class does not have a valid constructor"))
            case Success(_) => Left(RuntimeException(s"Class $replFilterClassName is not of type ReplFilter"))
            case Failure(error: ClassNotFoundException) => Left(RuntimeException(s"REPL filter class can't be located on classpath"))
            case Failure(error) => Left(error)
        case None => Left(RuntimeException("The setting -replfilter does not exist"))
    isInitialized_ = true
    returnValue

  def isInitialized: Boolean = isInitialized_