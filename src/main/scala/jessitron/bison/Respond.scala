package jessitron.bison

import scalaz.stream._

object Respond {

  def responder: Process1[Message,Message] = process1.lift {
    m: Message =>
      m match {
        case RespondTo(i) =>
          chooseBestResponse(i)
        case other => other
      }
  }

  def chooseBestResponse(incoming: IncomingTweet): Message = {
    val maxResponseLength = 140 - incoming.from.size
    val suitableOpinions = incoming.opinions.filter(hasSuggestionShorterThan(maxResponseLength))
    if (suitableOpinions.isEmpty)
      Notification("No suitable response to", incoming)
    else {
      val highestRankingSuggestion = suitableOpinions.maxBy{_.points}.suggestedText.get
      val responseTweet = OutgoingTweet(s"${incoming.from} ${highestRankingSuggestion}", Some(incoming.id))
      TweetThis(responseTweet, Some(incoming))
    }
  }

  private def hasSuggestionShorterThan(maxResponseLength: Int): Opinion => Boolean = {o: Opinion =>
    o.suggestedText.map(_.size < maxResponseLength).getOrElse(false)
  }

}
