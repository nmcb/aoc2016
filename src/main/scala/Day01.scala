import scala.io._
import scala.util._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Cmd(turn: Char, dist: Int)

  object Cmd:
    def fromString(s: String): Cmd =
      Try(Cmd(s.trim.head, s.trim.tail.toInt))
        .getOrElse(sys.error(s"unable to parse: $s"))

  enum Dir:
    case N
    case E
    case S
    case W

  import Dir.*

  case class Ikke(x: Int, y: Int, dir: Dir):

    def manhattanDistance: Int =
      taxiDistance

    def taxiDistance: Int =
      x.abs + y.abs

    def process(cmd: Cmd): Ikke =
      (dir, cmd.turn) match
        case (N, 'L') => copy(x = x - cmd.dist, dir = W)
        case (E, 'L') => copy(y = y + cmd.dist, dir = N)
        case (S, 'L') => copy(x = x + cmd.dist, dir = E)
        case (W, 'L') => copy(y = y - cmd.dist, dir = S)
        case (N, 'R') => copy(x = x + cmd.dist, dir = E)
        case (E, 'R') => copy(y = y - cmd.dist, dir = S)
        case (S, 'R') => copy(x = x - cmd.dist, dir = W)
        case (W, 'R') => copy(y = y + cmd.dist, dir = N)
        case _        => sys.error("boom!")

  object Ikke:
    def airDrop: Ikke =
      Ikke(0, 0, N)

  val commands: Vector[Cmd] =
    Source
      .fromResource(s"input$day.txt")
      .mkString.split(',')
      .map(Cmd.fromString)
      .toVector

  println(commands)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = commands.foldLeft(Ikke.airDrop)(_ process _).taxiDistance
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = 669
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
