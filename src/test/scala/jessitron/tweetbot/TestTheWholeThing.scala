package jessitron.tweetbot

import scalaz.stream._
import scalaz.concurrent.Task

import scala.concurrent.duration._

// OK this isn't a spec yet
object TestTheWholeThing {

  def main(args: Array[String]) {

    val inputFromFile = SearchInput("fake", new SearchInputSpec.FileFetcher)
    val liveInput = SearchInput("#codemesh")

    val outputChannel:Sink[Task, Message] =
      io.stdOut map { _ compose {(a: Message) => a.toString + "\n"} }

    val liveOutput = TwitterConnection.outputChannel()

    val p = Con4mationBison.agree(liveInput, liveOutput, maxTweets(args),
      15 seconds)

    (p through outputChannel).run.run
  }

  def maxTweets(args: Array[String]): Int = {
    10
  }

}
