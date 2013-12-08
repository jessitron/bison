package jessitron.bison

package object ranker {

  case class RankerState(tweetsSeen: Int = 0,
                         tweetsRanked: Int = 0,
                         pointsGivenOut: Double = 0.0,
                         recommendationsAccepted: Int = 0,
                         targetAveragePoints: Double = 1
                        ) {
    def rankedATweet(points: Score) =
      copy(tweetsSeen = tweetsSeen + 1,
           tweetsRanked = tweetsRanked + 1,
           pointsGivenOut = pointsGivenOut + points)
    def passedATweet() = copy(tweetsSeen = tweetsSeen + 1)
    def likelyAmountOfPoints: Score =
      if (tweetsRanked < 1) targetAveragePoints
      else targetAveragePoints * (tweetsSeen / tweetsRanked)
  }

}
