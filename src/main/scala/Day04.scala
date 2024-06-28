import scala.io.*

object Day04 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Room(underlying: String):

    def name: String =
      underlying.takeWhile(c => !c.isDigit)

    def id: Int =
      underlying.filter(_.isDigit).toInt

    def checksum: String =
      underlying match
        case s"$nameAndId[$checksum]" => checksum
        case _                        => sys.error(s"no checksum: $underlying")

    def isValid: Boolean =

      implicit val ordering: Ordering[(Char,Int)] =
        Ordering
          .by[(Char,Int),Int](_._2)
          .reverse
          .orElse(Ordering
            .by[(Char,Int),Int](_._1))

      val common: List[Char] =
        name
          .filterNot(_ == '-')
          .groupMapReduce(identity)(_ => 1)(_ + _)
          .toList
          .sorted
          .map(_._1)
          .take(5)

      checksum.forall(common.contains)

    def decrypt: String =
      def rotate(c: Char, i: Int): Char =
        if c > 'z' then rotate('a', i) else if i == 0 then c else rotate((c + 1).toChar, i - 1)

      name.map:
        case '-' => ' '
        case c   => rotate(c, id)


  object Room:
    def fromLine(s: String): Room = Room(s)

  val rooms: List[Room] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Room.fromLine)
      .toList

  val start1: Long = System.currentTimeMillis
  val answer1: Int = rooms.filter(_.isValid).map(_.id).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = rooms.find(_.decrypt.trim == "northpole object storage").get.id
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
