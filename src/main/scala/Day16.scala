import scala.annotation.tailrec

object Day16 extends App:

  @tailrec
  def dragon(s: String, length: Int): String =
    if s.length >= length then
      s.take(length)
    else
      dragon(s + '0' + s.reverse.map(c => if c == '0' then '1' else '0'), length)

  @tailrec
  def checksum(s: String): String =
    val result = s.grouped(2).map(t => if t(0) == t(1) then '1' else '0').mkString
    if result.length % 2 == 0 then checksum(result) else result

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val start1  = System.currentTimeMillis
  val answer1 = checksum(dragon("10001001100000001", 272))
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = checksum(dragon("10001001100000001", 35651584))
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
