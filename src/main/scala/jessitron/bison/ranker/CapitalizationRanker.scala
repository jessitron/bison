package jessitron.bison.ranker

import scalaz.stream._
import jessitron.bison._

object CapitalizationRanker {
  def apply(): Process1[IncomingTweet, IncomingTweet] =
    process1.lift { (in) =>
      if (in.tweet.text.startsWith("I"))
        in.addMyTwoCents(Opinion(1.0, Some("I Agree Completely")))
      else
        in.addMyTwoCents(Opinion(-1.0))
    }
}
