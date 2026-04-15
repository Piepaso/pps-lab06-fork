package it.unibo.pps.ex2

enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

/**
 * An interface modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the
 * system does not keep track of the identity of reviewers
 *
 */
trait ConferenceReviewing:
  /**
   * @param article
   * @param scores
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  /**
   * @param article
   * @param relevance
   * @param significance
   * @param confidence
   * @param fin
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @param article
   * @param question
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Int, question: Question): List[Int]

  /**
   * @param article
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles: Set[Int]

  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

class ConferenceReviewingImpl() extends ConferenceReviewing:
  import Question.*
  case class Review(scores: Map[Question, Int])
  private var reviews: Map[Int, List[Review]] = Map()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews += (article -> (Review(scores)::reviews.getOrElse(article, Nil)))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    loadReview(article, Map(RELEVANCE -> relevance, SIGNIFICANCE -> significance, CONFIDENCE -> confidence, FINAL -> fin))

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews(article).map(r => r.scores(question)).sorted

  override def averageFinalScore(article: Int): Double = funAvgFinalScore(reviews(article))

  override def acceptedArticles: Set[Int] = filterAcceptedArticle().keySet

  override def sortedAcceptedArticles: List[(Int, Double)] =
    filterAcceptedArticle().map(e => (e._1, funAvgFinalScore(e._2))).toList.sortBy(t => t._2)

  override def averageWeightedFinalScoreMap: Map[Int, Double] =
    reviews.map(e => e._1 -> avgScore(e._2)(r => r.scores(CONFIDENCE) * r.scores(FINAL)) / 10)

  private def funAvgFinalScore(l: List[Review]): Double = avgScore(l)(r => r.scores(FINAL))

  private def avgScore(l: List[Review])(f: Review => Double): Double = l.map(r => f(r)).sum / l.size

  private def filterAcceptedArticle() =
    reviews.filter(l => funAvgFinalScore(l._2) > 5 && l._2.exists(r => r.scores(RELEVANCE) >= 8))
