package jessitron.tweetbot

import scalaz.stream._
import scalaz.concurrent.Task

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
    (process1.lift { string: String =>
      string.asJson.convertTo[SearchResults]
    }) |> resultsToTweets

  def resultsToTweets: Process1[SearchResults, IncomingTweet] =
    Process.await1 flatMap { results: SearchResults =>
      Process.emitSeq(results.statuses)
    }

  trait Fetcher {
    def fetchBody(url: String, params: Map[String,String]): String
  }

  def apply(queryString: String, fetcher: Fetcher = new RealFetcher):
  Process[Task, String] =  {
    val searchUrl = "http://api.twitter.com/1.1/search/tweets.json"
    def go(params:Map[String,String]) :  Process[Task, String] = {
       Process.suspend {
         val body = fetcher.fetchBody(searchUrl, params)
         // TODO: more than one signal
         // TODO: fallback to file
         Process.emit(body)
       }
    }
    // TODO url-encode the query string
    go(Map("q" -> queryString,"result_type"->"recent"))
  }

  class RealFetcher extends Fetcher {
    import org.scribe.builder._
    import org.scribe.builder.api._
    import org.scribe.model._
    import org.scribe.oauth._

    def fetchBody(url: String, params: Map[String,String]) = {
      val service = new ServiceBuilder().provider(classOf[TwitterApi]).
                  apiKey("yKvmb3FXTzjDM9SVBQXg").
                  apiSecret("cs7ecBCyAP4CXQuFErwMnDXG1Q4nqmOrcFxgeDRiSAs").
                  build()
      val bisonToken =
        new Token("1546486698-Q5tiMgTLxYeC7mI5LRcdKONS29eT7zUivYZbAjT",
                  "F0vCPChOuMs13Wws610YGJTPUOgDHlenMimEMwR17LhTg")
      val request = new OAuthRequest(Verb.GET, url)
      params.foreach { case (key, value) => request.addQuerystringParameter(key, value)}
      service.signRequest(bisonToken, request)
      val response = request.send()
      response.getBody()
    }
  }
}
