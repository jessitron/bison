package jessitron.tweetbot

import scalaz.stream._
import scalaz.concurrent.Task

import spray.json._
import DefaultJsonProtocol._

case class AccessToken(consumerKey: String, consumerSecret: String, userKey: String, userSecret: String)
object AccessToken {
  import spray.json._
  import DefaultJsonProtocol._
  implicit val format: JsonFormat[AccessToken] = jsonFormat4(apply)
}

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
    parseJson[SearchResults] |> resultsToTweets

  def parseJson[T : JsonReader]: Process1[String, T] =
    (process1.lift { string: String =>
      string.asJson.convertTo[T]
    })

  def resultsToTweets: Process1[SearchResults, IncomingTweet] =
    Process.await1 flatMap { results: SearchResults =>
      Process.emitSeq(results.statuses)
    }

  trait Fetcher {
    def fetchBody(url: String, params: Map[String,String]): Task[String]
  }

  def apply(queryString: String, fetcher: Fetcher = new RealFetcher):
  Process[Task, String] =  {
    val searchUrl = "http://api.twitter.com/1.1/search/tweets.json"
    def go(params:Map[String,String]) :  Process[Task, String] = {
         // TODO: more than one signal
         // TODO: fallback to file
       Process.eval { fetcher.fetchBody(searchUrl, params) }
    }
    // TODO url-encode the query string
    go(Map("q" -> queryString,"result_type"->"recent"))
  }

  class RealFetcher extends Fetcher {
    import org.scribe.builder._
    import org.scribe.builder.api._
    import org.scribe.model._
    import org.scribe.oauth._

    lazy val accessToken = TwitterConnection.accessTokenSource

    def fetchBody(url: String, params: Map[String,String]) = {
      accessToken map { token =>
        val service = new ServiceBuilder().provider(classOf[TwitterApi]).
                    apiKey(token.consumerKey).
                    apiSecret(token.consumerSecret).
                    build()
        val bisonToken =
          new Token(token.userKey, token.userSecret)
        val request = new OAuthRequest(Verb.GET, url)
        params.foreach { case (key, value) => request.addQuerystringParameter(key, value)}
        service.signRequest(bisonToken, request)
        val response = request.send()
        response.getBody()
      }
    }
  }
}

object TwitterConnection {

  val keyFile = "keys.json"

  def accessTokenSource = {
      val inputFile = getClass.getClassLoader().getResourceAsStream(keyFile)
      val p = SearchInput.streamToJson(inputFile) |> SearchInput.parseJson[AccessToken]
      p.runLast.map { _.getOrElse(throw new RuntimeException("No access token obtained"))}
  }

}
