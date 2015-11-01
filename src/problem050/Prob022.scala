package problem050

/**
 * @author ahaljh
 */
object Prob022 {
  def readfile(filename: String): String = 
    scala.io.Source.fromFile(filename).mkString
    
  def sumAlpha(word: String): Int =
    word.toCharArray.map(c => (c - 'A'+1)).sum
    
  def main(args: Array[String]): Unit = {
    val content = readfile("names.txt").split(",")
                      .map(s => s.replace("\"", "")).sorted
                      .map(s => sumAlpha(s))
    
    for (i <- 0 until content.length) 
      yield content(i)*(i+1)
    
    println((for (i <- 0 until content.length) yield content(i)*(i+1)).sum)

  }
}