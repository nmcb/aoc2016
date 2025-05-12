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
    assertResult("8c35d1ab")(actual = Day05.answer2)
  }
  test("Day06") {
    assertResult("afwlyyyq")(actual = Day06.answer1)
    assertResult("bhkzekao")(actual = Day06.answer2)
  }
  test("Day07") {
    assertResult(118)(actual = Day07.answer1)
    assertResult(260)(actual = Day07.answer2)
  }
  test("Day08") {
    assertResult(110)(actual = Day08.answer1)
    assertResult("""####...##.#..#.###..#..#..##..###..#....#...#..##.
                   |...#....#.#..#.#..#.#.#..#..#.#..#.#....#...#...#.
                   |..#.....#.####.#..#.##...#....#..#.#.....#.#....#.
                   |.#......#.#..#.###..#.#..#....###..#......#.....#.
                   |#....#..#.#..#.#.#..#.#..#..#.#....#......#..#..#.
                   |####..##..#..#.#..#.#..#..##..#....####...#...##.."""
                   .stripMargin)(actual = Day08.answer2)
  }
  test("Day09") {
    assertResult(112830)(actual = Day09.answer1)
    assertResult(10931789799L)(actual = Day09.answer2)
  }
  test("Day10") {
    assertResult(98)(actual = Day10.answer1)
    assertResult(4042)(actual = Day10.answer2)
  }
