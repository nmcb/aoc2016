import scala.io.*
import scala.util.*

object Day08 extends App:

  val day: String = this.getClass.getName.drop(3).init

  enum Op:
    case Rectangle(sizeX: Int, sizeY: Int)
    case RotateCol(index: Int, by: Int)
    case RotateRow(index: Int, by: Int)

  object Op:
    def fromString(s: String): Op =
      s match
        case s"rect ${sizeX}x${sizeY}"        => Rectangle(sizeX = sizeX.toInt, sizeY = sizeY.toInt)
        case s"rotate column x=$index by $by" => RotateCol(index = index.toInt, by = by.toInt)
        case s"rotate row y=$index by $by"    => RotateRow(index = index.toInt, by = by.toInt)

  val operations: List[Op] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Op.fromString)
      .toList

  object Screen:
    def empty: Screen =
      Screen(List.fill(6, 50)('.'))


  case class Screen(pixels: List[List[Char]]):
    import Op.*

    val sizeX: Int = pixels.head.size
    val sizeY: Int = pixels.size

    def countLit: Int =
      pixels.map(_.count(_ == '#')).sum

    override def toString: String =
      pixels.map(_.mkString).mkString("\n")

    def transpose: Screen =
      Screen(pixels.transpose)

    def rectangle(sizeX: Int, sizeY: Int): Screen =
      val (top, bottom) = pixels.splitAt(sizeY)
      val updated = top.map(row => List.fill(sizeX)('#') :++ row.drop(sizeX))
      Screen(updated :++ bottom)

    def shiftRow(index: Int, by: Int): Screen =
      val (top, bottom) = pixels.splitAt(index)
      val (row, rest)   = bottom.splitAt(1)
      val shifted       = Iterator.continually(row.head).flatten.drop(sizeX - by).take(sizeX).toList
      Screen(top :+ shifted :++ rest)

    infix def process(op: Op): Screen =
      op match
        case Rectangle(sizeX, sizeY) => rectangle(sizeX, sizeY)
        case RotateRow(index, by)    => shiftRow(index, by)
        case RotateCol(index, by)    => transpose.shiftRow(index, by).transpose

  val start1: Long = System.currentTimeMillis
  val answer1: Int = operations.foldLeft(Screen.empty)(_ process _).countLit
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long    = System.currentTimeMillis
  val answer2: String = operations.foldLeft(Screen.empty)(_ process _).toString
  println(s"Answer day $day part 2:\n${answer2} [${System.currentTimeMillis - start2}ms]")