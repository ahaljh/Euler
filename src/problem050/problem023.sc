package problem050

object problem023 {
  
  // prob021
  def getSumOfDivisor(number: Int): Int =
    (1 to number/2).filter(n => number%n==0).sum  //> getSumOfDivisor: (number: Int)Int
    
  val dMap = (2 to 10000).map(n => (n, getSumOfDivisor(n))).filter(r => (r._2>1 && r._2<=10000)).toMap
                                                  //> dMap  : scala.collection.immutable.Map[Int,Int] = Map(2163 -> 1165, 8607 -> 
                                                  //| 3553, 645 -> 411, 892 -> 676, 69 -> 27, 5385 -> 3255, 5810 -> 6286, 7375 -> 
                                                  //| 1985, 2199 -> 737, 8062 -> 4538, 3021 -> 1299, 8536 -> 9104, 1322 -> 664, 16
                                                  //| 65 -> 1299, 5509 -> 795, 5686 -> 2846, 1036 -> 1092, 9982 -> 8450, 2822 -> 1
                                                  //| 714, 7304 -> 7816, 9131 -> 421, 2630 -> 2122, 6085 -> 1223, 3873 -> 1295, 41
                                                  //| 88 -> 5612, 1586 -> 1018, 8618 -> 4822, 1501 -> 99, 2452 -> 1846, 9405 -> 93
                                                  //| 15, 7373 -> 175, 8930 -> 8350, 7766 -> 4978, 3962 -> 2854, 5422 -> 2714, 133
                                                  //| 7 -> 199, 1718 -> 862, 2094 -> 2106, 6836 -> 5134, 5469 -> 1827, 9208 -> 807
                                                  //| 2, 3944 -> 4156, 1411 -> 101, 7427 -> 1069, 5365 -> 1475, 6387 -> 2133, 629 
                                                  //| -> 55, 8186 -> 4096, 3883 -> 365, 5116 -> 3844, 6405 -> 5499, 9458 -> 4732, 
                                                  //| 5561 -> 151, 6979 -> 1005, 2612 -> 1966, 4094 -> 2386, 6167 -> 889, 1024 -> 
                                                  //| 1023, 5918 -> 3802, 1469 -> 127, 8398 -> 6722, 365 -> 79, 5088 -> 8520, 9273
                                                  //|  -> 4263, 2744 -> 3256, 
                                                  //| Output exceeds cutoff limit.
  
  dMap.filter(x => (dMap.getOrElse(x._2, 0) == x._1)).filter(x => x._1 != x._2).keys.sum
                                                  //> res0: Int = 31626
  
  // prob023
  val abundants = (2 to 28123).filter(n => getSumOfDivisor(n) > n)
                                                  //> abundants  : scala.collection.immutable.IndexedSeq[Int] = Vector(12, 18, 20,
                                                  //|  24, 30, 36, 40, 42, 48, 54, 56, 60, 66, 70, 72, 78, 80, 84, 88, 90, 96, 100
                                                  //| , 102, 104, 108, 112, 114, 120, 126, 132, 138, 140, 144, 150, 156, 160, 162,
                                                  //|  168, 174, 176, 180, 186, 192, 196, 198, 200, 204, 208, 210, 216, 220, 222, 
                                                  //| 224, 228, 234, 240, 246, 252, 258, 260, 264, 270, 272, 276, 280, 282, 288, 2
                                                  //| 94, 300, 304, 306, 308, 312, 318, 320, 324, 330, 336, 340, 342, 348, 350, 35
                                                  //| 2, 354, 360, 364, 366, 368, 372, 378, 380, 384, 390, 392, 396, 400, 402, 408
                                                  //| , 414, 416, 420, 426, 432, 438, 440, 444, 448, 450, 456, 460, 462, 464, 468,
                                                  //|  474, 476, 480, 486, 490, 492, 498, 500, 504, 510, 516, 520, 522, 528, 532, 
                                                  //| 534, 540, 544, 546, 550, 552, 558, 560, 564, 570, 572, 576, 580, 582, 588, 5
                                                  //| 94, 600, 606, 608, 612, 616, 618, 620, 624, 630, 636, 640, 642, 644, 648, 65
                                                  //| 0, 654, 660, 666, 672, 678, 680, 684, 690, 696, 700, 702, 704, 708, 714, 720
                                                  //| , 726, 728, 732, 736, 73
                                                  //| Output exceeds cutoff limit.
  def canMakeFromAbundant(number: Int): Boolean = {
    val lessAbundant = abundants.filter(n => n < number)
    for (a <- lessAbundant; b <- lessAbundant)
      if (number == a + b) return true
    false
  }                                               //> canMakeFromAbundant: (number: Int)Boolean
  
  (1 to 28123).filter(n => !canMakeFromAbundant(n)).sum
                                                  //> res1: Int = 4179871
    
}