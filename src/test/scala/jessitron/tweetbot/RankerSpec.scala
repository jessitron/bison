package jessitron.tweetbot

import scalaz.stream._

import org.scalacheck._
import Prop._

object RankerSpec extends Properties("Rankers") {
  import Common._

  property("The random ranker always adds an opinion") = forAll(incomingTweet){
    incoming: IncomingTweet =>
      val p = Process(incoming) |> Rankers.randomo
      val output = p.toList collect { case i: IncomingTweet => i}

      (output.size ?= 1) :| "Something always comes out" &&
      (output.head.opinions.size ?= (1 + incoming.opinions.size)) :| "One more opinon"
  }

}
