package dotty.tools.scaladoc.util;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.function.Consumer;
import java.nio.charset.Charset;

/** This code is mostly using public snippets and tries to mimic sbt-io api. */
public class IO {
  public static void delete(File pathToBeDeleted) throws IOException {
    Files.walkFileTree(pathToBeDeleted.toPath(),
      new SimpleFileVisitor<Path>() {
        @Override
        public FileVisitResult postVisitDirectory(
          Path dir, IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult visitFile(
          Path file, BasicFileAttributes attrs)
          throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }
    });
  }

  public static void foreachFileIn(Path dir, Consumer<Path> op) throws IOException {
    Files.walkFileTree(dir, new SimpleFileVisitor<Path>() {
        @Override
        public FileVisitResult visitFile(
          Path file, BasicFileAttributes attrs)
          throws IOException {
            op.accept(file);
            return FileVisitResult.CONTINUE;
        }
    });
  }

  public static String read(Path path) throws IOException {
    return new String(Files.readAllBytes(path), Charset.defaultCharset());
  }

  public static String read(String path) throws IOException {
    return read(Paths.get(path));
  }
}
