package dotty.dokka;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

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
}
