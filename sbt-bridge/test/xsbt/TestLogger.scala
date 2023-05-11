package xsbt

import java.util.function.Supplier

import xsbti.*

class TestLogger extends Logger:
  override def debug(msg: Supplier[String]): Unit = ()
  override def error(msg: Supplier[String]): Unit = ()
  override def info(msg: Supplier[String]): Unit = ()
  override def warn(msg: Supplier[String]): Unit = ()
  override def trace(exception: Supplier[Throwable]): Unit = ()
