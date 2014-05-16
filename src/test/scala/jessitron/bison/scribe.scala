package jessitron.bison

import java.util.Scanner

import org.scribe.builder._
import org.scribe.builder.api._
import org.scribe.model._
import org.scribe.oauth._
import scodec.bits.ByteVector

// This is totally right out of scribe, modified for my own purposes
object AuthorizationSetup
{
  val PROTECTED_RESOURCE_URL = "https://api.twitter.com/1.1/search/tweets.json"
  val KEY_FILE_LOCATION = "src/main/resources/keys.json"

  def main(args: Array[String])
  {
    val in = new Scanner(System.in)
    println("First, go to https://dev.twitter.com/apps/ and create an app.")
    println("You'll need its API Key and its API Secret.")
    println("")
    System.out.print("Enter the API key >>")
    val consumerKey = in.nextLine()
    System.out.print("Enter the API secret >>")
    val consumerSecret = in.nextLine()

    val service = new ServiceBuilder().debug().provider(classOf[TwitterApi]).
                  apiKey(consumerKey).
                  apiSecret(consumerSecret).
                  build()

   System.out.println("=== Twitter's OAuth Workflow ===")
   System.out.println()

 //Obtain the Request Token
 val accessToken = {
   System.out.println("Fetching the Request Token...")
   val requestToken = service.getRequestToken()
   System.out.println("Got the Request Token!")
   System.out.println()

   System.out.println("Now go and authorize Scribe here:")
   System.out.println(service.getAuthorizationUrl(requestToken))
   System.out.println("And paste the verifier here")
   System.out.print(">>")
   val verifier = new Verifier(in.nextLine())
   System.out.println()


 // Trade the Request Token and Verfier for the Access Token
   System.out.println("Trading the Request Token for an Access Token...")
   service.getAccessToken(requestToken, verifier)
}
 System.out.println("Got the Access Token!")
 System.out.println("(if you're curious it looks like this: " + accessToken + ")")
 System.out.println()


 //Now let's go and ask for a protected resource!
 System.out.println("Now we're going to access a protected resource...")
 val request = new OAuthRequest(Verb.GET, PROTECTED_RESOURCE_URL)
 request.addQuerystringParameter("q","#CPL14")
 service.signRequest(accessToken, request)
 val response = request.send()
 System.out.println("Got it!  Lets see what we found...")
 System.out.println()
 val body = response.getBody()
 System.out.println(body)


     parseTweets(body)
     writeKeysToFile(AccessToken(consumerKey, consumerSecret, accessToken.getToken, accessToken.getSecret))

     println(s"Success! Configuration written to $KEY_FILE_LOCATION")

 }

 case class AccessToken(consumerKey: String, consumerSecret: String, userKey: String, userSecret: String)
 object AccessToken {
   import spray.json._
   import DefaultJsonProtocol._
   implicit val format: JsonFormat[AccessToken] = jsonFormat4(apply)
 }

 private def writeKeysToFile(accessToken: AccessToken) {
   import scalaz.stream.{Process => P}

   val fileWriter = scalaz.stream.io.fileChunkW(KEY_FILE_LOCATION)
   def toBytes(s: String): ByteVector = ByteVector.view(s.getBytes)

   val p = P(accessToken).toSource |> writeJson |> scalaz.stream.process1.lift(toBytes) through fileWriter
   p.run.run
 }

 import spray.json._
 import DefaultJsonProtocol._
   def writeJson[T : JsonWriter]: scalaz.stream.Process1[T, String] =
   (scalaz.stream.process1.lift { obj: T =>
       obj.toJson.prettyPrint
   })

 private def parseTweets(body: String) = {
   import scalaz.concurrent.Task
   import scalaz.stream.{Process => P}
   (P(body) |> SearchInput.jsonToTweets) onFailure P.eval { Task.delay{ println(s"Unable to parse response: $body")}}
 }

}
