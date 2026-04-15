package it.unibo.pps.ex2

import org.junit.Assert._
import Question.*

class TestConferenceReviewing:
  private var cr: ConferenceReviewing = ConferenceReviewing()
  @org.junit.Before def init(): Unit =
    // si ricordi che l'ordine delle domande è: relevance, significance, confidence, final
    cr.loadReview(1, 8, 8, 6, 8) // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
    cr.loadReview(1, 9, 9, 6, 9) // 5.4
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6
    val map = Map(RELEVANCE -> 8, SIGNIFICANCE -> 8, CONFIDENCE -> 7, FINAL -> 8)
    cr.loadReview(4, map)
    cr.loadReview(5, 6, 6, 6, 10) // 6.0
    cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @org.junit.Test def testOrderedScores(): Unit =
    println(cr.acceptedArticles)
    // l'articolo 2 ha preso su RELEVANCE i due voti 4,9
    assertEquals(cr.orderedScores(2, Question.RELEVANCE), List(4, 9))
    // e simile per gli altri
    assertEquals(cr.orderedScores(4, Question.CONFIDENCE), List(6, 7, 8))
    assertEquals(cr.orderedScores(5, Question.FINAL), List(10, 10))

  @org.junit.Test def testAverageFinalScore(): Unit =
    // l'articolo 1 ha preso voto medio su FINAL pari a 8.5, con scarto massimo 0.01
    assertEquals(cr.averageFinalScore(1), 8.5, 0.01)
    // e simile per gli altri
    assertEquals(cr.averageFinalScore(2), 7.5, 0.01)
    assertEquals(cr.averageFinalScore(3), 3.5, 0.01)
    assertEquals(cr.averageFinalScore(4), 7.0, 0.01)
    assertEquals(cr.averageFinalScore(5), 10.0, 0.01)

  @org.junit.Test def testAcceptedArticles(): Unit =
    // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assertEquals(cr.acceptedArticles, Set(1, 2, 4))

  @org.junit.Test def testSortedAcceptedArticles(): Unit =
    // articoli accettati, e loro voto finale medio
    assertEquals(cr.sortedAcceptedArticles, List((4, 7.0), (2, 7.5), (1, 8.5)))

  @org.junit.Test def optionalTestAverageWeightedFinalScore(): Unit =
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals(cr.averageWeightedFinalScoreMap(1), (4.8 + 5.4) / 2, 0.01)
    // e simile per gli altri
    assertEquals(cr.averageWeightedFinalScoreMap(2), (9.0 + 6.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(3), (0.9 + 1.6) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(4), (3.6 + 5.6 + 5.6) / 3, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(5), (6.0 + 7.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.size, 5)


