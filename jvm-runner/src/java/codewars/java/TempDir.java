package codewars.java;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;

import static java.nio.file.Files.createTempDirectory;

public class TempDir {

    private static void delete(Path path) throws IOException {
        if (!Files.exists(path)) return;

        Files.walkFileTree(path, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc)
                    throws IOException {
                Files.deleteIfExists(dir);
                return super.postVisitDirectory(dir, exc);
            }

            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
                    throws IOException {
                Files.deleteIfExists(file);
                return super.visitFile(file, attrs);
            }
        });
    }

    private static FileAttribute<Set<PosixFilePermission>> posixPermissions(String permissions) {
        Set<PosixFilePermission> perms = PosixFilePermissions.fromString(permissions);
        return PosixFilePermissions.asFileAttribute(perms);
    }

    private static void deleteOnShutdown(final Path path) {
         Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                try {
                    delete(path);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        });
    }

    public static File create(String dir, String prefix, String permissions) throws IOException {
        final Path tempDirPath = createTempDirectory(Paths.get(dir), prefix, posixPermissions(permissions));
        deleteOnShutdown(tempDirPath);
        return tempDirPath.toFile();
    }

    public static File create(String prefix, String permissions) throws IOException {
        final Path tempDirPath = createTempDirectory(prefix, posixPermissions(permissions));
        deleteOnShutdown(tempDirPath);
        return tempDirPath.toFile();
    }

    // TODO: If permissions are not specified, use umask to get system defaults somehow
    public static File create(String prefix) throws IOException {
        return create(prefix, "rwx------");
    }

}
