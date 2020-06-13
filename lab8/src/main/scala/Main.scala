import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import cats.{Applicative, Id, Monad}
import cats.syntax.all._
import cats.instances.list._
import scala.jdk.CollectionConverters._


trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait GetFiles[F[_], Dir, File] {
  def getFiles(dir: Dir): F[List[File]]
}

trait GetFileName[F[_], File] {
  def getFileName(file: File): F[String]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(file: File, dir: Dir): F[File]
}

trait Printer[F[_], File] {
  def printFileName(file: File): F[Unit]
}



class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               getFiles: GetFiles[F, Dir, File],
                               getFileName: GetFileName[F, File],
                               moveFile: MoveFile[F, Dir, File],
                               printer: Printer[F, File]) {
  def run(dir: Dir, dirName: String, fileNames: List[String]): F[Unit] = for {
    newDir <- mkDir.mkDir(dir, dirName)
    _ <- fileNames.traverse(s => mkFile.mkFile(newDir, s))
    files <- getFiles.getFiles(newDir)
    _ <- files.traverse(f => printer.printFileName(f))
    firstLetters <- files.traverse(f => getFileName.getFileName(f).map(s => s.slice(0, 1)))
    dirs <- firstLetters.traverse(s => mkDir.mkDir(newDir, s)) // We suppose that if the directory already exists mkDir will not create a duplicate
    _ <- files.zip(dirs).traverse(p =>  moveFile.moveFile(p._1, p._2))
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path]  with GetFiles[F, Path, Path] with GetFileName[F, Path] with MoveFile[F, Path, Path] {
  override def mkDir(dir: Path, name: String): F[Path] = {
    val newDir = dir.resolve(name)
    if (Files.isDirectory(newDir))
      newDir.pure[F]
    else
      Files.createDirectories(newDir).pure[F]
  }

  override def mkFile(dir: Path, name: String): F[Path] = {
    val file = dir.resolve(name)
    if (Files.isRegularFile(file))
      file.pure[F]
    else
      Files.createFile(file).pure[F]
  }

  override def getFiles(dir: Path): F[List[Path]] =
    Files.walk(dir).iterator().asScala.filter(Files.isRegularFile(_)).toList.pure[F]

  override def getFileName(file: Path): F[String] =
    file.getFileName.toString.pure[F]

  override def moveFile(file: Path, dir: Path): F[Path] =
    Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printFileName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

object Main {
  def main(args: Array[String]): Unit = {
    implicit val fileSystem: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]
    program.run(Paths.get("."), "test_dir", List("foo", "bar", "baz"))
  }
}