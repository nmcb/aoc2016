import scala.io.*
import scala.util.*

import java.security.MessageDigest

object Day05 extends App:

  val day: String = this.getClass.getName.drop(3).init

  val id = "ffykfhsq"

  def hex(bytes: Array[Byte]): String =
    bytes.map("%02X" format _).mkString

  val digest: MessageDigest =
    MessageDigest.getInstance("MD5")

  def decrypt(counter: Long = 0L, code: String = ""): String =
    digest.reset()
    val hash   = digest.digest(s"$id$counter".getBytes("UTF-8"))
    val chars  = hex(hash)

    if chars.startsWith("00000") then
      val next = code + chars(5)
      println(s"counter=$counter, chars=$chars, code=$next")
      if next.length == 8 then
        next
      else
        decrypt(counter = counter + 1, code = next)
    else
      decrypt(counter = counter + 1, code = code)

  val start1: Long    = System.currentTimeMillis
  val answer1: String = decrypt().toLowerCase
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

//  val start2: Long = System.currentTimeMillis
//  val answer2: Int = ???
//  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
