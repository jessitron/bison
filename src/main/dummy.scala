import scalaz.streams._

object Dummy {
   def main(args: Array[String]) {
     val converter: Task[Unit] =
         io.linesR("testdata/fahrenheit.txt").
          filter(s => !s.trim.isEmpty && !s.startsWith("//")).
                 map(line => fahrenheitToCelsius(line.toDouble).toString).
                      intersperse("\n").
                           pipe(process1.utf8Encode).
                                to(io.fileChunkW("testdata/celsius.txt")).
                                     run

                                     // at the end of the universe...
                                     // val u: Unit = converter.run
   }
}
