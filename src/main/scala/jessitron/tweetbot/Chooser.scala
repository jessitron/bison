package jessitron.tweetbot

sealed trait Message
case object TimeToTweet extends Message

case class IncomingTweet(tweet: TweetDetail,
                         opinions: Seq[Opinion]) extends Message {
   def totalScore = opinions map {_.points} sum
}

case class TweetThis(tweet: TweetDetail,
                     inReplyTo: Option[IncomingTweet] = None) extends Message

case class RespondTo(tweet:IncomingTweet) extends Message

case class Opinion(points: Score)
case class TweetDetail(text: TweetContents)

import scalaz.stream._

object Chooser {

  def tweetPicker(poolSize: Int = 5): Process1[Message, Message] = {
    def go(pool: TweetPool): Process1[Message, Message] = {
      import Process._
      await1 flatMap { message: Message =>
        message match {
          case i: IncomingTweet => go(pool.absorb(i))
          case t@ TimeToTweet =>
            pool.findBest match {
              case None => emit(TimeToTweet) ++ go(pool)
              case Some((tweet, newPool)) => emit(respondTo(tweet)) ++ go(newPool)
            }
          case m => emit(m) ++ go(pool)
        }
      }
    }
    go(new TweetPool(poolSize))
  }

  private def respondTo(incoming: IncomingTweet) : RespondTo =
    RespondTo(incoming)

  class TweetPool(size: Int, contents: Seq[IncomingTweet] = Vector()) {
    // There is probably a more efficient datastructure to use here. While poolSize is tiny it doesn't matter.
    private def copy(contents: Seq[IncomingTweet]) = new TweetPool(size, contents)
    def absorb(tweet: IncomingTweet): TweetPool =
      if (contents.size < size)
        copy(contents :+ tweet) // it fits
      else
        contents sortBy {_.totalScore} match {
          case Seq(lamest, rest@_*) if lamest.totalScore < tweet.totalScore =>
            copy(rest :+ tweet)
          case betterThanYou => this
        }

    def findBest: Option[(IncomingTweet, TweetPool)] =
      contents sortBy {_.totalScore * -1} match {
        case Seq() => None
        case Seq(head, tail@ _*) =>
          Some((head, copy(tail)))
      }
  }
}
