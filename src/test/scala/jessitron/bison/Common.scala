package jessitron.bison

import org.scalacheck._
import Gen._

/*
 * Generators used in testing
 */
object Common {

  implicit val tweetListGenerator: Arbitrary[List[IncomingTweet]] =
    Arbitrary(someIncomingTweets)

  val someIncomingTweets: Gen[List[IncomingTweet]] = smallNumberOf(incomingTweet)

  def smallNumberOf[A](g: Gen[A]): Gen[List[A]] = for {
    n <- smallInt
    list <- listOfN(n, g)
  } yield list

  val smallInt = choose(1, 7)

  val incomingTweet: Gen[IncomingTweet] = for {
    opinionCount <- choose(0,5)
    opinions <- listOfN(opinionCount, opinion)
    tweet <- tweetDetail
  } yield IncomingTweet(tweet, opinions)

  def word(n: Int): Gen[String] = listOfN(n, alphaChar) map {_.mkString}

  val tweetId: Gen[TweetId] = choose(10000, 10000000) map {_.toString}

  def wordOfRandomLength = for {
    n <- smallInt
    w <- word(n)
  } yield w

  val tweetText: Gen[String] = for {
    n <- choose(1, 20)
    words <- listOfN(n, wordOfRandomLength)
   } yield words.mkString(" ")

  val username: Gen[String] = for {
    i <- choose(2, 15)
    str <- word(i)
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
