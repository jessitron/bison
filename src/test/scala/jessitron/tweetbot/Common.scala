package jessitron.tweetbot

import org.scalacheck._
import Gen._

/*
 * Generators used in testing
 */
object Common {
  val smallInt = choose(1, 7)
  def alphaStr(n: Int): Gen[String] = listOfN(n, alphaChar) map {_.mkString}

  def smallNumberOf[A](g: Gen[A]): Gen[List[A]] = for {
    n <- smallInt
    list <- listOfN(n, g)
  } yield list

  val tweetText: Gen[String] = for {
    l <- choose(1, 120)
    str <- alphaStr(l)
   } yield str

  val tweetId: Gen[TweetId] = choose(10000, 10000000) map {_.toString}
  val username: Gen[String] = for {
    i <- choose(2, 15)
    str <- alphaStr(i)
  } yield str
  val twitterUser: Gen[TwitterUser] = username map {TwitterUser(_)}
  val tweetDetail: Gen[TweetDetail] = for {
    text <- tweetText
    id <- tweetId
    user <- twitterUser
  } yield TweetDetail(text, id, user)

  val suggestedText: Gen[SuggestedText] = frequency((10, value(None)),
                                                    (90, tweetText map {Some(_)}))

  val opinion: Gen[Opinion] = for {
    score <- choose(-100, 100) map {_.toDouble / 100}
    textOption <- suggestedText
  } yield Opinion(score, textOption)

  val incomingTweet: Gen[IncomingTweet] = for {
    opinionCount <- choose(0,5)
    opinions <- listOfN(opinionCount, opinion)
    tweet <- tweetDetail
  } yield IncomingTweet(tweet, opinions)

  val someIncomingTweets: Gen[List[IncomingTweet]] = smallNumberOf(incomingTweet)

  val someRespondTos: Gen[List[RespondTo]] = for {
    n <- smallInt
    list <- listOfN(n, incomingTweet)
  } yield (list map { RespondTo(_) })

  val outgoingTweet = tweetText map { OutgoingTweet(_)}
  val tweetThis: Gen[TweetThis] = for {
    tweet <- outgoingTweet
    inResponse <- frequency((9, incomingTweet map {Some(_)}),(1, None))
  } yield TweetThis(tweet, inResponse)

  val randomMessage: Gen[Message] = frequency((10, tweetThis),
                                              (1, value(TimeToTweet)))

  val assortedMessages: Gen[List[Message]] = smallNumberOf(randomMessage)
}
