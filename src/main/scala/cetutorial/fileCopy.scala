package cetutorial

import cats.effect.{IO, Resource}
import cats.syntax.all._
import java.io._

object fileCopy {
  def copy(origin: File, destination: File): IO[Long] = {
    /*
    inputOutputStreams(origin, destination).use { case(in, out) =>
      transfer(in, out)
    }
    */

    // この場合 inputOutputStreams は不要
    val inIO: IO[InputStream] = IO(new FileInputStream(origin))
    val outIO: IO[OutputStream] = IO(new FileOutputStream(destination))

    (inIO, outIO)            // リソースの取得
      .tupled                // (IO[InputStream], IO[OutputStream]) to IO[(InputStream, OutputStream)]
      .bracket {
        case (in, out) =>
          transfer(in, out)  // リソースの使用
      } {
        // このコードの場合、stage1 の取得で失敗するとこの stage3は実行されない点に注意
        // -> bracket より Resource 使うほうが良い
        case (in, out) =>    // リソースの開放
          (IO(in.close()), IO(out.close()))
          .tupled            // (IO[Unit], IO[Unit]) to IO[(Unit, Unit)]
          .handleErrorWith(_ => IO.unit).void
      }
  }

  def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] = {
    for {
      // blocking でアクションを作成している（入出力の場合は通常のIOよりこちらの方がよい）
      amount <- IO.blocking(origin.read(buffer, 0, buffer.size))
      // for内包表記を用いたループ
      count  <- if(amount > -1) IO.blocking(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
               else IO.pure(acc)
    } yield count
  }

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] = {
    transmit(origin, destination, new Array[Byte](1024 * 10), 0L)
  }

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
