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

  def chooseBestResponse(incoming: IncomingTweet): TweetThis = {
    val highestRankingSuggestion = incoming.opinions.filter{_.hasSuggestion}.maxBy{_.points}
    val responseTweet = TweetDetail(highestRankingSuggestion.suggestedText.get)
    TweetThis(responseTweet, Some(incoming))
  }

}
