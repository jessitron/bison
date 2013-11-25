package jessitron.tweetbot

import scalaz.stream._

import org.scalacheck._
import Prop._

object SearchInputSpec extends Properties("SearchInput") {

  val testInputFile = "someTweets.json"

  // this is a single example.
  property("can deserialize json into incoming tweets") =
    {
      import java.util.Date
      println(s"Starting ${new Date}")
      val inputFile = this.getClass.getClassLoader().getResourceAsStream(testInputFile)
      import scalaz._
      import scalaz.std.AllInstances._
      val source = ((Process(10000).toSource.repeat) through io.chunkR(inputFile))

      val bytesToString = process1.lift{ab: Array[Byte] => ab.map{_.toChar}.mkString}

      val process = (source |> bytesToString).foldMonoid |> SearchInput.jsonToTweets

      println(s"Running ${new Date}")
      val output = process.runLog.run
      println(s"Doning ${new Date}")

      println(output)
      true
    }



}
