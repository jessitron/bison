package jessitron.tweetbot


import scalaz.stream._

object Chooser {

  def tweetPicker(poolSize: Int = 5): Process1[Message, Message] = {
    import Process._
    def go(pool: TweetPool): Process1[Message, Message] = {
      await1[Message] flatMap {
          case i: IncomingTweet => go(pool.absorb(i))
          case t@ TimeToTweet =>
            val (newMessage, newPool) = pool.findBest
            emit(newMessage) ++ go(newPool)
          case m => emit(m) ++ go(pool)
      }
    }
    go(new TweetPool(poolSize))
  }

  private def respondTo(incoming: IncomingTweet) : RespondTo =
    RespondTo(incoming)

  class TweetPool(size: Int = 5, contents: Seq[IncomingTweet] = Vector()) {
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

    def findBest: (Message, TweetPool) =
      contents sortBy {_.totalScore * -1} match {
        case Seq() => (TimeToTweet, this) // propagate the message
        case Seq(head, tail@ _*) => (respondTo(head), copy(tail))
      }
  }
}
