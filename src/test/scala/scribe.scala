
import java.util.Scanner

import org.scribe.builder._
import org.scribe.builder.api._
import org.scribe.model._
import org.scribe.oauth._

object TwitterExample
{
  val PROTECTED_RESOURCE_URL = "http://api.twitter.com/1.1/search/tweets.json"

  def main(args: Array[String])
  {
    val service = new ServiceBuilder().provider(classOf[TwitterApi]).
                  apiKey("yKvmb3FXTzjDM9SVBQXg").
                  apiSecret("cs7ecBCyAP4CXQuFErwMnDXG1Q4nqmOrcFxgeDRiSAs").
                  build()
    val in = new Scanner(System.in)

   System.out.println("=== Twitter's OAuth Workflow ===")
   System.out.println()

 //Obtain the Request Token
 val accessToken = if (false) {
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
 else
   new Token("1546486698-Q5tiMgTLxYeC7mI5LRcdKONS29eT7zUivYZbAjT",
     "F0vCPChOuMs13Wws610YGJTPUOgDHlenMimEMwR17LhTg")
 System.out.println("Got the Access Token!")
 System.out.println("(if your curious it looks like this: " + accessToken + ")")
 System.out.println()


 //Now let's go and ask for a protected resource!
 System.out.println("Now we're going to access a protected resource...")
 val request = new OAuthRequest(Verb.GET, PROTECTED_RESOURCE_URL)
 request.addQuerystringParameter("q","#oredev")
 //request.addBodyParameter("status", "this is sparta!  *")
 service.signRequest(accessToken, request)
 val response = request.send()
 System.out.println("Got it!  Lets see what we found...")
 System.out.println()
 System.out.println(response.getBody())

 System.out.println()
 System.out.println("Thats it man!  Go and build something awesome with Scribe!  :)")
  }

 }
