import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult(288)(actual = Day01.answer1)
    assertResult(111)(actual = Day01.answer2)
  }
  test("Day02") {
    assertResult("38961")(actual = Day02.answer1)
  }
