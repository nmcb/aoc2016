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

  def decrypt1(counter: Int = 0, code: String = ""): String =
    digest.reset()
    val hash   = digest.digest(s"$id$counter".getBytes("UTF-8"))
    val chars  = hex(hash)

    if chars.startsWith("00000") then
      val next = code + chars(5)
      if next.length == 8 then
        next
      else
        decrypt1(counter = counter + 1, code = next)
    else
      decrypt1(counter = counter + 1, code = code)

  val start1: Long    = System.currentTimeMillis
  val answer1: String = decrypt1().toLowerCase
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  def decrypt2(counter: Int = 0, codes: Map[Int,Char] = Map.empty): String =
    digest.reset()
    val hash = digest.digest(s"$id$counter".getBytes("UTF-8"))
    val chars = hex(hash)

    if chars.startsWith("00000") then
      val index = chars(5) - '0'
      val char  = chars(6)

      if index >= 0 && index < 8 && !codes.contains(index) then
        val next = codes + (index -> char)
        if next.size == 8 then
          next.toList.sortBy(_._1).map(_._2).mkString
        else
          decrypt2(counter = counter + 1, codes = next)
      else
        decrypt2(counter = counter + 1, codes = codes)
    else
      decrypt2(counter = counter + 1, codes = codes)

  val start2: Long    = System.currentTimeMillis
  val answer2: String = decrypt2().toLowerCase
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")