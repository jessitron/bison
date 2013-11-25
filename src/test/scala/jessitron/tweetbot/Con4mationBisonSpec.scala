package jessitron.tweetbot

import scalaz.stream._
import scalaz.concurrent.Task

// OK this isn't a spec yet
object TestTheWholeThing {

  def main(args: Array[String]) {

    val sadInputFile = getClass.getClassLoader.getResourceAsStream("someTweets.json")
    val inputStreams = Process(sadInputFile).toSource

    val outputChannel:Sink[Task, Any] =
      io.stdOut map { _ compose {(a: Any) => a.toString + "\n"} }

    val p = Con4mationBison.agree(inputStreams, outputChannel, maxTweets(args))

    p.runLog.run

  }

  def maxTweets(args: Array[String]): Int = {
    10
  }

}
