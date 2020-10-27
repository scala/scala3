package dotty.dokka;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.zip.*;

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

  private static File newFile(File destinationDir, ZipEntry zipEntry) throws IOException {
        File destFile = new File(destinationDir, zipEntry.getName());
        
        String destDirPath = destinationDir.getCanonicalPath();
        String destFilePath = destFile.getCanonicalPath();
        
        if (!destFilePath.startsWith(destDirPath + File.separator)) {
            throw new IOException("Entry is outside of the target dir: " + zipEntry.getName());
        }
        
        return destFile;
    }

  public static File unzip(File fileZip, File destDir) throws IOException {
      byte[] buffer = new byte[1024];
      ZipInputStream zis = null;
      
      try {
        zis= new ZipInputStream(new FileInputStream(fileZip));
        ZipEntry zipEntry = zis.getNextEntry();
        while (zipEntry != null) {
            File newFile = newFile(destDir, zipEntry);
            FileOutputStream fos = new FileOutputStream(newFile);
            int len;
            while ((len = zis.read(buffer)) > 0) {
                fos.write(buffer, 0, len);
            }
            fos.close();
            zipEntry = zis.getNextEntry();
        }
        zis.closeEntry();
      } finally {
        if (zis != null) zis.close();
      }
      return destDir;
  }
}