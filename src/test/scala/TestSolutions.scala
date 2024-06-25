import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult(288)(actual = Day01.answer1)
    assertResult(111)(actual = Day01.answer2)
  }
  test("Day02") {
    assertResult("38961")(actual = Day02.answer1)
    assertResult("46C92")(actual = Day02.answer2)
  }
  test("Day03") {
    assertResult( 869)(actual = Day03.answer1)
    assertResult(1544)(actual = Day03.answer2)
  }
  test("Day04") {
    assertResult(185371)(actual = Day04.answer1)
    assertResult(   984)(actual = Day04.answer2)
  }
  test("Day05") {
    assertResult("c6697b55")(actual = Day05.answer1)
  }
