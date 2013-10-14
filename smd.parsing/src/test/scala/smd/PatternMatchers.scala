package smd

import org.scalatest.matchers.{MatchSucceeded, MatchResult, Matcher}

trait PatternMatchers {
  sealed abstract class PatternMatchResult
  object matched extends PatternMatchResult
  object unmatched extends PatternMatchResult

  def matchPattern(pattern: PartialFunction[Any, PatternMatchResult]): Matcher[Any] = new Matcher[Any] {
    def apply(left: Any): MatchResult =
      MatchResult(
        pattern.lift.apply(left) match {
          case Some(_ @ matched) => true
          case _ => false
        },
        "did not match the provided pattern",
        "should not have matched the provided pattern"
      )
  }
}
