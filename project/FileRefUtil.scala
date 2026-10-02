package sbt

import java.io.File

import sbt.internal.util.Attributed
import xsbti.{FileConverter, HashedVirtualFileRef}

extension (ref: HashedVirtualFileRef)
  def toFile(using conv: FileConverter): File =
    conv.toPath(ref).toFile()

extension (entry: Attributed[HashedVirtualFileRef])
  def toFile(using conv: FileConverter): File =
    conv.toPath(entry.data).toFile()

extension (f: File)
  def toFileRef(using conv: FileConverter): HashedVirtualFileRef =
    conv.toVirtualFile(f.toPath())
