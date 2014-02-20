package playfair

import java.io._
import scala.io.Source

/**
 * Main object to launch the program.
 * 
 * @author Zhishen Wen
 * @version Nov 10, 2013
 * CIS 554
 */
object Playfair {
  
  def main(args: Array[String]) { run() }
  
  /**
   * Launches this program: a REPL for user interaction
   */
  private def run() {
    val rsp = readLine("What would you like me to do? encode (e), decode (d), or quit (q)\n");
    rsp match {
      case r if r.matches("[eE]+") => playfair(true)
      case r if r.matches("[dD]+") => playfair(false)
      case r if r.matches("[qQ]+") => println("Bye."); exit()
      case _ => println("I can't understand " + rsp + " !")
    }
    println()
    run()
  }
  
  /**
   * Launches Playfair encoding/decoding:
   * Prompts user to enter a file path, then ask for a keyword.
   * If the file path/keyword is invalid, it returns; otherwise 
   * displays the encoded/decoded content to the console.
   */
  private def playfair(enc: Boolean) {
    try {
      val fileName = readLine("Please enter your file path: ")
      val file = new File(fileName)
      val lines = Source.fromFile(file).getLines().toList
      val keyword = readLine("Please specify a key word: ")
      if (!keyword.matches("[a-zA-Z]+")) {
        println("Invalid keyword!")
        return
      }
      println("\nThe original content of file " + file.getName() + ":")
      for (line <- lines) println(line)
      println("\nThe keyword you specify: " + keyword)
      val coder = new Coder(keyword);
      val text = lines.mkString
      println("--------------------------------------------------------")
      if (enc) {
        println("The encoded content:")
        println(coder.encode(text))
      }
      else {
        println("The decoded content:")
        println(coder.decode(text))
      }
    }
    catch {
      case e: IOException => println("Cannot access this file!")
    }
  }
  
}