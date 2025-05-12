import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01")
    assertResult(288)(Day01.answer1)
    assertResult(111)(Day01.answer2)

  test("Day02")
    assertResult("38961")(Day02.answer1)
    assertResult("46C92")(Day02.answer2)

  test("Day03")
    assertResult( 869)(Day03.answer1)
    assertResult(1544)(Day03.answer2)

  test("Day04")
    assertResult(185371)(Day04.answer1)
    assertResult(   984)(Day04.answer2)

  test("Day05")
    assertResult("c6697b55")(Day05.answer1)
    assertResult("8c35d1ab")(Day05.answer2)

  test("Day06")
    assertResult("afwlyyyq")(Day06.answer1)
    assertResult("bhkzekao")(Day06.answer2)

  test("Day07")
    assertResult(118)(Day07.answer1)
    assertResult(260)(Day07.answer2)

  test("Day08")
    assertResult(110)(Day08.answer1)
    assertResult("""####...##.#..#.###..#..#..##..###..#....#...#..##.
                   |...#....#.#..#.#..#.#.#..#..#.#..#.#....#...#...#.
                   |..#.....#.####.#..#.##...#....#..#.#.....#.#....#.
                   |.#......#.#..#.###..#.#..#....###..#......#.....#.
                   |#....#..#.#..#.#.#..#.#..#..#.#....#......#..#..#.
                   |####..##..#..#.#..#.#..#..##..#....####...#...##.."""
                   .stripMargin)(Day08.answer2)

  test("Day09")
    assertResult(112830)(Day09.answer1)
    assertResult(10931789799L)(Day09.answer2)

  test("Day10")
    assertResult(98)(Day10.answer1)
    assertResult(4042)(Day10.answer2)
  
