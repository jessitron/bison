package jessitron.tweetbot

import scalaz.stream._
import spray.json._
import DefaultJsonProtocol._

object SearchInput {

  def streamToJson(input: java.io.InputStream) = {
      val bytesToString = process1.lift{ab: Array[Byte] => ab.map{_.toChar}.mkString}
      import scalaz._
      import scalaz.std.AllInstances._
      val source = ((Process(10000).toSource.repeat) through io.chunkR(input))

      (source |> bytesToString).foldMonoid
  }

  case class SearchResults(statuses: Seq[IncomingTweet])
  object SearchResults {
    implicit val jsonFormat: JsonFormat[SearchResults] = jsonFormat1(apply)
  }

  val jsonToTweets: Process1[String, IncomingTweet] =
    Process.await1 flatMap { string: String =>
      val parsed = string.asJson.convertTo[SearchResults]
      Process.emitSeq(parsed.statuses)
    }

}
