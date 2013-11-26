package jessitron.tweetbot

import scalaz.stream._
import scalaz.concurrent.Task

import scala.concurrent.duration._

// OK this isn't a spec yet
object TestTheWholeThing {

  def main(args: Array[String]) {

    val inputFromFile = SearchInput("fake", new SearchInputSpec.FileFetcher)

    val outputChannel:Sink[Task, Any] =
      io.stdOut map { _ compose {(a: Any) => a.toString + "\n"} }

    val p = Con4mationBison.agree(inputFromFile, outputChannel, maxTweets(args), 500 millis)

    p.runLog.run

  }

  def maxTweets(args: Array[String]): Int = {
    10
  }

}
