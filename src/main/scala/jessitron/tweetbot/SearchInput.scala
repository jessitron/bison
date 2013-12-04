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
    import Process._
    val searchUrl = "http://api.twitter.com/1.1/search/tweets.json"
    val NextMaxId = """"next_results": "?max_id=([0-9]*)&q""".r
    def nextPage(params: Map[String,String])(body: String) = emit(body) ++ (NextMaxId.findFirstIn(body) match {
      case Some(NextMaxId(max)) => go(params + ("max_id" -> max))
      case None => halt
    })
    def go(params:Map[String,String]) :  Process[Task, String] = {
      // TODO: fallback to file
      val body = fetcher.fetchBody(searchUrl, params)
      await(body)(nextPage(params))
    }
    // TODO url-encode the query string
    go(Map("q" -> queryString,"result_type"->"recent", "include_entities"->"0"))
  }

  class RealFetcher extends Fetcher {
    import org.scribe.builder._
    import org.scribe.builder.api._
    import org.scribe.model._
    import org.scribe.oauth._


    def fetchBody(url: String, params: Map[String,String]) = {
      TwitterConnection.accessTokenSource .map
      { _.getOrElse(throw new RuntimeException("No access token obtained"))}.map
      { token =>
        val request = new OAuthRequest(Verb.GET, url)
        params.foreach { case (key, value) => request.addQuerystringParameter(key, value)}
        TwitterConnection.signRequest(token)(request)
        val response = request.send()
        response.getBody()
      }
    }
  }
}

object TwitterConnection {
    import org.scribe.builder._
    import org.scribe.builder.api._
    import org.scribe.model._
    import org.scribe.oauth._

  val keyFile = "keys.json"
  lazy val accessToken = TwitterConnection.accessTokenSource

  def accessTokenSource = {
      val inputFile = getClass.getClassLoader().getResourceAsStream(keyFile)
      val p = SearchInput.streamToJson(inputFile) |> SearchInput.parseJson[AccessToken]
      p.runLast
  }

  def signRequest(token: AccessToken)(request: OAuthRequest) {
        val service = new ServiceBuilder().provider(classOf[TwitterApi]).
                    apiKey(token.consumerKey).
                    apiSecret(token.consumerSecret).
                    build()
        val bisonToken =
          new Token(token.userKey, token.userSecret)
        service.signRequest(bisonToken, request)
  }

  def outputChannel(): Channel[Task, Message, Message] = {
    accessTokenSource.run match {
      case Some(token) =>
        io.channel {
          case m@ TweetThis(ogt, irt) => Task.delay {
            reallyTweet(token, ogt)
            Notification("Tweeted", m) }
          case m => Task.delay { m }
        }
      case None =>
        io.channel {
          case m@ TweetThis(ogt, irt) => Task.delay {
            println(s">> pretending to tweet $ogt");
            Notification("Tweeted", m) }
          case m => Task.delay { m }
        }
    }
  }

  val statusUpdateURL = "https://api.twitter.com/1.1/statuses/update.json"
  def reallyTweet(token: AccessToken, tweet: OutgoingTweet) {
        val request = new OAuthRequest(Verb.POST, statusUpdateURL)
        val requiredParams = Map("status" -> tweet.text)
        val params = if (tweet.replyTo.nonEmpty)
           requiredParams + ("in_reply_to_status_id" -> tweet.replyTo.get)
        else requiredParams
        params.foreach { case (key, value) => request.addBodyParameter(key, value)}
        TwitterConnection.signRequest(token)(request)
        val response = request.send()
        println(response.getBody())
  }

}
