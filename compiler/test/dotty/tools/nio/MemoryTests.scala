package dotty.tools.nio

import org.junit.*
import org.junit.Assert.*
import dotty.tools.nio.*

import scala.io.Codec

class MemoryTests:
  @Test
  def `Contents are persisted until deletion`(): Unit =
    val container = FileContainer.createInMemory("container")
    assertEquals("container", container.name)
    assertTrue(container.getFile("x").isEmpty)

    val file = container.getOrCreateFile("x")
    file.writeText("Hello!", Codec.UTF8)

    val file2 = container.getOrCreateFile("x")
    assertEquals("Hello!", file2.readText(Codec.UTF8))
    file2.delete()
    assertEquals("", file.readText(Codec.UTF8))

    assertTrue(container.getFile("x").isEmpty)

  @Test
  def `Deleting a subdirectory works`(): Unit =
    val container = FileContainer.createInMemory("container")
    assertTrue(container.getContainer("sub").isEmpty)
    val sub = container.getOrCreateContainer("sub")
    assertTrue(container.getContainer("sub").nonEmpty)
    sub.deleteRecursively()
    assertTrue(container.getContainer("sub").isEmpty)