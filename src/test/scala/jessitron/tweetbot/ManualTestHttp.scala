package jessitron.tweetbot

import scalaz.stream._
import scalaz.concurrent.Task

object ManuallyTestHttp {

 val anythingToStdOut:Sink[Task, Any] =
      io.stdOut map { _ compose {(a: Any) => a.toString + "\n"} }

  def main(args: Array[String]) {

    val p = Process(true).toSource.tee(SearchInput("#scala"))(tee.when).observe(io.stdOut) |> SearchInput.jsonToTweets
    val output = p through anythingToStdOut
    output.runLast.run
  }

}
