package jessitron.tweetbot

import scalaz.stream._

import org.scalacheck._
import Prop._

object SearchInputSpec extends Properties("SearchInput") {

  val testInputFile = "someTweets.json"

  class FileFetcher extends SearchInput.Fetcher {
    def fetchBody(url: String, params: Map[String,String]):String = {
      val inputFile = getClass.getClassLoader().getResourceAsStream(testInputFile)
      val json = SearchInput.streamToJson(inputFile)
      json.runLast.run.getOrElse("nothing read!")
    }

  }

  // this is a single example.
  property("can deserialize json into incoming tweets") =
    {
      val process = SearchInput("whatever", new FileFetcher) |> SearchInput.jsonToTweets
      val output = process.runLog.run

      (output.size == 15) :| "There are 15 tweets in that file" &&
      output.head.tweet.text.contains("Check out the video from my #oredev talk")
    }



}
