package jessitron.tweetbot

import scalaz.stream._

object Respond {

  def responder: Process1[Message,Message] = process1.lift {
    m: Message =>
      m match {
        case RespondTo(i) if containsSuggestion(i) =>
          chooseBestResponse(i)
        case other => other
      }
  }

  def containsSuggestion(incoming:IncomingTweet):Boolean = incoming.opinions.exists{_.hasSuggestion}

  def chooseBestResponse(incoming: IncomingTweet): Message = {
    val maxResponseLength = 140 - incoming.from.size
    val suitableOpinions = incoming.opinions.filter(hasSuggestionShorterThan(maxResponseLength))
    if (suitableOpinions.isEmpty)
      Notification("No suitable response to", incoming)
    else {
      val highestRankingSuggestion = suitableOpinions.maxBy{_.points}.suggestedText.get
      val responseTweet = OutgoingTweet(s"${incoming.from} ${highestRankingSuggestion}")
      TweetThis(responseTweet, Some(incoming))
    }
  }

  private def hasSuggestionShorterThan(maxResponseLength: Int): Opinion => Boolean = {o: Opinion =>
    o.suggestedText.map(_.size < maxResponseLength).getOrElse(false)
  }

}
