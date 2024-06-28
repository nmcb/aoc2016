import scala.io.*

object Day03 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  def isValid(triangle: List[Int]): Boolean =
    triangle.permutations.forall(l => l(0) + l(1) > l(2))

  def fromLine(s: String): List[Int] =
    val sides = s.trim.split("\\s+")
    List(sides(0).toInt, sides(1).toInt, sides(2).toInt)

  val input: List[List[Int]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(fromLine)
      .toList

  val start1: Long = System.currentTimeMillis
  val answer1: Int = input.filter(isValid).size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def triangulate(todo: List[Int], acc: List[List[Int]] = List.empty): List[List[Int]] =
    todo match
      case a :: b :: c :: rest => triangulate(rest, acc :+ List(a, b, c))
      case _ => acc

  val start2: Long    = System.currentTimeMillis
  val answer2: Int = triangulate(input.transpose.flatten).filter(isValid).size
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
