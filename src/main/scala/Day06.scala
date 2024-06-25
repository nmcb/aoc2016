import java.security.MessageDigest
import scala.io.*
import scala.util.*

object Day06 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Counter(counts: Map[Char,Int] = Map.empty):
    infix def add(char: Char): Counter =
      Counter(counts.updatedWith(char){
        case Some(count) => Some(count + 1)
        case None        => Some(1)
      })

    def max: Char =
      counts.toList.maxBy(_._2)._1

    def min: Char =
      counts.toList.minBy(_._2)._1

  object Counter:
    def empty: Counter =
      Counter()

  val input: List[List[Char]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toList)
      .toList

  val counted: List[Counter] =
    input
      .transpose
      .map(_.foldLeft(Counter.empty)(_ add _))

  val start1: Long    = System.currentTimeMillis
  val answer1: String = counted.map(_.max).mkString
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long    = System.currentTimeMillis
  val answer2: String = counted.map(_.min).mkString
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")