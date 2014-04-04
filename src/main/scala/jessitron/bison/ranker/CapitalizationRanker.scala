package jessitron.bison.ranker

import scalaz.stream._
import jessitron.bison._

object CapitalizationRanker {
  def apply1(): Process1[IncomingTweet, IncomingTweet] =
    process1.lift { (in) =>
      in.addMyTwoCents(Opinion(1.0, Some("I Agree Completely")))
    }

  def apply2(): Process1[IncomingTweet, IncomingTweet] =
    process1.lift { (in) =>
      if (in.tweet.text.startsWith("I"))
        in.addMyTwoCents(Opinion(1.0, Some("I Agree Completely")))
      else
        in.addMyTwoCents(Opinion(-1.0))
    }

  def apply(): Process1[IncomingTweet, IncomingTweet] =
    process1.lift { (in) =>
      val firstchars:Seq[Char] = in.tweet.text.split(" ").filter(_.nonEmpty).map(_.charAt(0))
      val caps = firstchars.count(_ <= 'Z')
      val percentCaps = caps.toDouble / firstchars.size
      val score = (percentCaps - 0.5) * 2
      val suggestion = if (score > 0) Some("I Agree Completely") else None

      in.addMyTwoCents(Opinion(score, suggestion))
    }
}
