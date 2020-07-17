package effect

import java.io.{File, FileInputStream, FileOutputStream, FilterOutputStream, InputStream, OutputStream}

import cats.effect.{IO, Resource}
import cats.implicits._

class CopyFile {

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO(new FileInputStream(f))
    } { inStream =>
      IO(inStream.close()).handleErrorWith(_ => IO.unit)
    }

  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(f))
    } { outStream =>
      IO(outStream.close()).handleErrorWith(_ => IO.unit)
    }

  def inputOutputStreams(in: File, out: File): Resource[IO, (InputStream, OutputStream)] =
    inputStream(in).flatMap(is =>
      outputStream(out).map(os =>
        (is, os)))

  def transmit(origin: InputStream, destination: OutputStream, buffer: Array[Byte], acc: Long): IO[Long] =
    for {
      amount <- IO(origin.read(buffer, 0, buffer.size))
      count <-
        if (amount > -1) IO(destination.write(buffer, 0, amount)) >> transmit(origin, destination, buffer, acc + amount)
        else IO.pure(acc)
    } yield count

  def transfer(origin: InputStream, destination: OutputStream): IO[Long] =
    for {
      buffer <- IO(new Array[Byte](1024 * 10))
      total <- transmit(origin, destination, buffer, 0L)
    } yield total

  def copy(origin: File, destination: File): IO[Long] =
    inputOutputStreams(origin, destination).use { case (in, out) =>
      transfer(in, out)
    }

  def copyUseBracket(origin: File, dest: File): IO[Long] = {
    val inIO: IO[InputStream] = IO(new FileInputStream(origin))
    val outIO: IO[OutputStream] = IO(new FileOutputStream(dest))


    (inIO, outIO)
      .tupled
      .bracket {
        case (in, out) => transfer(in, out)
      } {
        case (in, out) => (IO(in.close()), IO(out.close()))
          .tupled
          .handleErrorWith(_ => IO.unit)
          .void
      }


  }

}
