package xsbt

import xsbti.compile.*

class TestDependencyChanges extends DependencyChanges:
  def isEmpty(): Boolean = ???
  def modifiedBinaries(): Array[java.io.File] = ???
  def modifiedClasses(): Array[String] = ???
  def modifiedLibraries(): Array[xsbti.VirtualFileRef] = ???
