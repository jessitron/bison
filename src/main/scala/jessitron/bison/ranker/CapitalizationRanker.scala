package jessitron.bison.ranker

import scalaz.stream._
import jessitron.bison._

object CapitalizationRanker {
  def apply(): Process1[IncomingTweet, IncomingTweet] =
    process1.lift { (in) =>
      in.withOpinion(1.0, "I Agree Completely")
    }

}
