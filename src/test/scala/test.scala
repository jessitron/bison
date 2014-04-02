import scalaz.stream._
import Process._
import scalaz.concurrent.Task
import scala.concurrent.duration._


// git bisect identifies the offending commit as
// https://github.com/scalaz/scalaz-stream/commit/721716ed7af0c126593e9ee227c0f36f21c5b7ed
object Test {

  def main(args: Array[String]) {
    // this could be a constant process, this doesn't appear to be required
    val triggers: Process[Task, Duration] = Process.awakeEvery(1.second).take(10)

    // this can also be a constant, delay not required
    val randomStrings: Process[Task, String] = Process.eval{ Task.delay{ "boo!" }}.repeat.take(50)

    // no SOE without the tee AND the every -- removing one fixes it
    val slowStrings: Process[Task,String] = Process.every(400.millis).tee(randomStrings)(tee.when)

    // the suspicious wye
    def combined: Process[Task, Any] = slowStrings.wye(triggers)(wye.merge[Any])

    // SOE. Free monads all over the stack, entering go() each time
    combined.runLog.run
  }
}
