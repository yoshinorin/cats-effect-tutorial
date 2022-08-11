package cetutorial

import cats.effect.{IO, Resource}
import java.io._

object fileCopy {
  def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination).use { case(in, out) =>
      transfer(in, out)
    }

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] = ???

  def inputStream(f: File): Resource[IO, FileInputStream] = {
    Resource.make {
      IO.blocking(new FileInputStream(f))
    } { inStream =>
      IO.blocking(inStream.close()).handleErrorWith(_ => IO.unit)
    }
  }

  def outputStream(f: File): Resource[IO, FileOutputStream] = {
    Resource.make {
      IO.blocking(new FileOutputStream(f))
    } { outStream =>
      IO.blocking(outStream.close()).handleErrorWith(_ => IO.unit)
    }
  }

  def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] = {
    for {
      inStream <- inputStream(in)
      outStream <- outputStream(out)
    } yield (inStream, outStream)
  }
}
