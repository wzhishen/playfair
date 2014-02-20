package playfair

/**
 * Class that handles Playfair encoding/decoding.
 * 
 * @author Zhishen Wen
 * @version Nov 10, 2013
 * CIS 554
 */
class Coder(keyword: String) {
  
  /**
   * Generates a key grid for encoding/decoding. 
   * The key grid is simply stored as a String.
   * @return A grid for the keyword.
   */
  def generateGrid() = {
    val letters = (for (c <- 'a' to 'z' if c != 'j') yield c).mkString
    (normalize(keyword) + letters).distinct
  }
  
  /**
   * Generates digraphs (pairs of letters) according to
   * Playfair's roles.
   * @return A list of letter pairs.
   */
  def generateDigraphs(letters: List[Char]): List[String] = {
    letters match {
      case first :: second :: tail if first != second =>
        "" + first + second :: generateDigraphs(tail)
      case 'x' :: 'x' :: tail =>
        "" + 'x' + 'q' :: generateDigraphs('x' :: tail)
      case first :: second :: tail =>
        "" + first + 'x' :: generateDigraphs(second :: tail)
      case first :: Nil =>
        "" + first + 'z' :: Nil
      case _ => Nil
    }
  }
  
  /**
   * Encodes input text.
   * @return A formatted String representing
   * the encoded content.
   */
  def encode(plainText: String) = {
    val res = process(generateDigraphs(normalize(plainText).toList), generateGrid, false)
    format(groupByFive(res), 1).trim
  }
  
  /**
   * Decodes input text.
   * @return A formatted String representing
   * the decoded content.
   */
  def decode(secretText: String) = {
    val res = process(generateDigraphs(normalize(secretText).toList), generateGrid, true)
    format(groupByFive(res), 1).trim
  }
  
  /**
   * Normalizes input text: only retain letters 
   * and convert them to lowercase.
   * @return A normalized String.
   */
  def normalize(text: String) = text.replaceAll("[^a-zA-Z]", "").toLowerCase
  
  /**
   * Formats a List of String, grouping them as ten items per line,
   * with proper spaces/newlines/ appended.
   * @return A formatted String.
   */
  private def format(text: List[String], n: Int): String = {
    if (text.isEmpty) ""
    else if (n % 10 == 0) text.head + "\n" + format(text.tail, n + 1)
    else text.head + " " + format(text.tail, n + 1)
  }
  
  /**
   * Groups a List of Chars by five, concats them to a String
   * @return A List of grouped Chars as Strings. 
   */
  private def groupByFive(text: List[Char]): List[String] = {
    text match {
      case c1 :: c2 :: c3 :: c4 :: c5 :: tail =>
        "" + c1 + c2 + c3 + c4 + c5 :: groupByFive(tail)
      case cs => cs.mkString :: Nil
    }
  }
  
  /**
   * Processes a List of Digraphs with given keyword, in a normal (encoding) 
   * or reversed (decoding) order specified by 'reversed' param.
   * @return A List of processed Chars.
   */
  private def process(digraphs: List[String], grid: String, reversed: Boolean): List[Char] = {
    if (digraphs.isEmpty) return Nil
    val first = grid.indexOf(digraphs.head.charAt(0))
    val second = grid.indexOf(digraphs.head.charAt(1))
    if (row(first) == row(second)) {
      grid.charAt(nextRowPos(first, reversed)) :: grid.charAt(nextRowPos(second, reversed)) :: process(digraphs.tail, grid, reversed)
    }
    else if (col(first) == col(second)) {
      grid.charAt(nextColPos(first, reversed)) :: grid.charAt(nextColPos(second, reversed)) :: process(digraphs.tail, grid, reversed)
    }
    else {
      grid.charAt(nextRecPos(first, second)) :: grid.charAt(nextRecPos(second, first)) :: process(digraphs.tail, grid, reversed)
    }
  }
  
  /**
   * @return Position for the next letter in the same row.
   */
  private val nextRowPos = (n: Int, reversed: Boolean) => {
    if (reversed) (n + 4) % 5 + n / 5 * 5
    else (n + 1) % 5 + n / 5 * 5
  }
  
  /**
   * @return Position for the next letter in the same column.
   */
  private val nextColPos = (n: Int, reversed: Boolean) => {
    if (reversed) (n + 20) % 25
    else (n + 5) % 25
  }
  
  /**
   * @return Position for the next letter in the same rectangle.
   */
  private val nextRecPos = (first: Int, second: Int) => first - (col(first) - col(second))
  
  /**
   * @return The relative row index, given a absolute index in the grid.
   */
  private val row = (n: Int) => n / 5
  
  /**
   * @return The relative column index, given a absolute index in the grid.
   */
  private val col = (n: Int) => n % 5
  
}