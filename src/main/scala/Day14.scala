import java.security.MessageDigest

object Day14 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val md5  = MessageDigest.getInstance("MD5")
  val salt = "zpqevtbw"

  val Three = "(.)\\1{2}".r.unanchored
  val Five  = "(.)\\1{4}".r.unanchored

  def solve(hash: Int => String): Int =

    def validate(window: Seq[(String,Int)]): Boolean =
      window.head match
        case (Three(t), _) =>
          window.tail.exists:
            case (Five(f), _) if f == t => true
            case _                      => false
        case _                          => false

    val (_, index) = Iterator.from(0).map(hash).zipWithIndex.sliding(1 + 1000).filter(validate).drop(63).next.head
    index



  val HEX_CHARS = "0123456789abcdef".toCharArray
  extension (bytes: Array[Byte])
    def toHexString: String =
      val sb: StringBuffer = StringBuffer(bytes.length * 2)
      for b <- bytes do
        sb.append(HEX_CHARS((b & 0xF0) >> 4))
        sb.append(HEX_CHARS(b & 0x0F))
      sb.toString

  def hash(string: String): String =
    md5.digest(string.getBytes).toHexString

  val start1  = System.currentTimeMillis
  val answer1 = solve(index => hash(s"$salt$index"))
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def stretchedHash(string: String): String =
    Iterator.iterate(string)(hash).drop(1 + 2016).next

  val start2  = System.currentTimeMillis
  val answer2 = solve(index => stretchedHash(s"salt$index"))
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
