package jessitron.tweetbot

import scalaz.stream._

import org.scalacheck._
import Prop._

object SearchInputSpec extends Properties("SearchInput") {

  val testInputFile = "someTweets.json"

  // this is a single example.
  property("can deserialize json into incoming tweets") =
    {
      val inputFile = getClass.getClassLoader().getResourceAsStream(testInputFile)
      val bytesToString = process1.lift{ab: Array[Byte] => ab.map{_.toChar}.mkString}
      import scalaz._
      import scalaz.std.AllInstances._
      val source = ((Process(10000).toSource.repeat) through io.chunkR(inputFile))

      val process = (source |> bytesToString).foldMonoid |> SearchInput.jsonToTweets
      val output = process.runLog.run

      (output.size == 15) :| "There are 15 tweets in that file" &&
      output.head.tweet.text.contains("Check out the video from my #oredev talk")
    }



}
