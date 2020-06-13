import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, Paths, SimpleFileVisitor}

import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class Test extends AnyFlatSpec with Matchers {
  val tmpDir: Path = Files.createDirectories(Paths.get("./tmp"))
  implicit val fileSystem: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
  val program = new Program[Id, Path, Path]

  program.run(tmpDir, "dir", List("ab", "aa", "bcd", "12"))

  val dir: Path = tmpDir.resolve("dir")
  Files.isDirectory(dir) shouldBe true

  val dirA: Path = dir.resolve("a")
  val dirB: Path = dir.resolve("b")
  val dir1: Path = dir.resolve("1")

  val dirs = List(dirA, dirB, dir1)
  dirs.foreach(d => Files.isDirectory(d) shouldBe true)

  val files = List(dirA.resolve("ab"),
    dirA.resolve("aa"),
    dirB.resolve("bcd"),
    dir1.resolve("12"))

  files.foreach(f => Files.isRegularFile(f) shouldBe true)

  Files.exists(dir.resolve("aa")) shouldBe false

  Files.walkFileTree(
    tmpDir,
    new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, ex: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    }
  )
}