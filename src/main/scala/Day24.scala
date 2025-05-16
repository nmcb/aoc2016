import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):

    infix def +(that: Pos): Pos =
      copy(x = x + that.x, y = y + that.y)

    def neighbours: Set[Pos] =
      Set(Pos(-1, 0), Pos(1, 0), Pos(0, -1), Pos(0, 1)).map(_ + this)

  val(grid: Set[Pos], nodes: Map[Int,Pos]) =
    val lines = Source.fromResource("AdventOfCode2016/Day24.txt").getLines.toVector
    val grid  = for y <- lines.indices ; x <- lines.head.indices if lines(y)(x) != '#'  yield Pos(x, y)
    val nodes = for y <- lines.indices ; x <- lines.head.indices if lines(y)(x).isDigit yield lines(y)(x).asDigit -> Pos(x, y)
    (grid.toSet, nodes.toMap)

  def steps(grid: Set[Pos], start: Pos, end: Pos): Int =
    import scala.collection.*
    val steps = mutable.Map(start -> 0)
    val todo  = mutable.Queue(start)

    while todo.nonEmpty do
      val current = todo.dequeue
      if current == end then return steps(current)
      val update  = steps(current) + 1
      current.neighbours.intersect(grid).foreach: next =>
        if !steps.contains(next) || update < steps(next) then
          steps(next) = update
          todo.enqueue(next)
    sys.error("boom!")

  def network(grid: Set[Pos], nodes: Map[Int,Pos]): Map[Int,Map[Int,Int]] =
    nodes.transform: (_, start) =>
      nodes.transform: (_, end) =>
        steps(grid, start, end)

  def tsp(graph: Map[Int, Map[Int,Int]], routes: Iterator[Seq[Int]]): Int =
    routes.map(_.sliding(2).map(next => graph(next.head)(next.last)).sum).min

  def solve1(grid: Set[Pos], nodes: Map[Int,Pos]): Int =
    val graph = network(grid, nodes)
    val routes = (1 to nodes.keys.max).permutations.map(_.prepended(0))
    tsp(graph, routes)

  val start1  = System.currentTimeMillis
  val answer1 = solve1(grid, nodes)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(grid: Set[Pos], nodes: Map[Int, Pos]): Int =
    val graph = network(grid, nodes)
    val routes = (1 to nodes.keys.max).permutations.map(_.prepended(0).appended(0))
    tsp(graph, routes)

  val start2  = System.currentTimeMillis
  val answer2 = solve2(grid, nodes)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
