package cetutorial

import cats.effect.IO
import java.io.File

object fileCopy {
  def copy(origin: File, destination: File): IO[Long] = ???
}
