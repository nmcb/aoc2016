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

    infix def process(cmd: Cmd): List[Ikke] =
      (dir, cmd.turn) match
        case (N, 'L') => (1 to cmd.dist).map(dist => copy(x = x - dist, dir = W)).toList
        case (E, 'L') => (1 to cmd.dist).map(dist => copy(y = y + dist, dir = N)).toList
        case (S, 'L') => (1 to cmd.dist).map(dist => copy(x = x + dist, dir = E)).toList
        case (W, 'L') => (1 to cmd.dist).map(dist => copy(y = y - dist, dir = S)).toList
        case (N, 'R') => (1 to cmd.dist).map(dist => copy(x = x + dist, dir = E)).toList
        case (E, 'R') => (1 to cmd.dist).map(dist => copy(y = y - dist, dir = S)).toList
        case (S, 'R') => (1 to cmd.dist).map(dist => copy(x = x - dist, dir = W)).toList
        case (W, 'R') => (1 to cmd.dist).map(dist => copy(y = y + dist, dir = N)).toList
        case _        => sys.error("boom!")

  object Ikke:
    def airDrop: Ikke =
      Ikke(0, 0, N)

  val commands: List[Cmd] =
    Source
      .fromResource(s"input$day.txt")
      .mkString.split(',')
      .map(Cmd.fromString)
      .toList

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    commands.foldLeft(List(Ikke.airDrop))(_.last process _).last.taxiDistance

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  def loop(commands: List[Cmd], path: List[Ikke]): Ikke =

    def twice(test: List[Ikke], visited: List[Ikke]): Option[Ikke] =
      test match
        case Nil                                                                     => None
        case h :: t if visited.find(ikke => ikke.x == h.x && ikke.y == h.y).nonEmpty => Some(h)
        case _ :: t                                                                  => twice(t, visited)

    val next = path.last.process(commands.head)
    if twice(next, path).nonEmpty then twice(next, path).get else loop(commands.tail :+ commands.head, path :++ next)

  val answer2: Int =
    loop(commands, List(Ikke.airDrop)).manhattanDistance


  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
