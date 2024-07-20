import scala.io.Source
import scala.collection.mutable

object Day10 extends App:

  val day: String = this.getClass.getName.drop(3).init

  sealed trait Target:
    val i: Int

  case class Bot(i: Int) extends Target
  case class Out(i: Int) extends Target

  enum Inst:
    case Init(bot: Bot, value: Int)
    case Sort(bot: Bot, low: Target, high: Target)

  import Inst.*

  private def solve(instructions: Seq[Inst]) =

    val targets: mutable.Map[Target,Set[Int]] =
      mutable.Map[Target,Set[Int]]().withDefaultValue(Set.empty)

    instructions.foreach:
      case Init(bot, value) =>
        targets += bot -> (targets(bot) + value)
      case _ =>

    val sorts: mutable.Map[Target,Set[Set[Int]]] =
      mutable.Map[Target,Set[Set[Int]]]().withDefaultValue(Set.empty)

    var changed: Boolean = true

    while (changed)
      changed = false
      targets.find(_._2.size == 2) match
        case Some((target, values)) =>
          instructions.find:
            case Sort(bot, _, _) => bot == target
            case _ => false
          match
            case Some(Sort(bot, low, high)) =>
              targets += low -> (targets(low) + values.min)
              targets += high -> (targets(high) + values.max)
              targets += bot -> Set()
              sorts += bot -> (sorts(bot) + values)
              changed = true
            case _ =>
        case None =>

    (targets, sorts)

  def part1(instructions: Seq[Inst], values: Set[Int]): Int =
    val targets = solve(instructions)._2
    targets.find(_._2.contains(values)).get._1.i

  def part2(instructions: Seq[Inst]): Int =
    val targets = solve(instructions)._1
    targets(Out(0)).head * targets(Out(1)).head * targets(Out(2)).head

  private val init =
    """value (\d+) goes to bot (\d+)""".r

  private val sort =
  """bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)""".r

  def targetFromString(s: String, i: Int): Target =
    s match
      case "bot" => Bot(i)
      case "output" => Out(i)

  def instFromString(s: String): Inst =
    s match
      case init(value, boti) =>
        Init(Bot(boti.toInt), value.toInt)
      case sort(boti, lowTarget, lowi, highTarget, highi) =>
        Sort(Bot(boti.toInt), targetFromString(lowTarget, lowi.toInt), targetFromString(highTarget, highi.toInt))

  def instructionsFromString(s: String): Seq[Inst] =
    s.linesIterator.map(instFromString).toSeq

  def solvePart1(input: String, values: Set[Int] = Set(61, 17)): Int =
    part1(instructionsFromString(input), values)

  def solvePart2(input: String): Int =
    part2(instructionsFromString(input))

  lazy val input: String =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solvePart1(input)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solvePart2(input)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

