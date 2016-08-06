package dmf.stream.quantiles



trait StreamQuantile {
  def insert(x: Double)
  def getQuantile(phi: Double): Double
}


class StreamQuantileTrivial extends StreamQuantile {
  val buffer = collection.mutable.ArrayBuffer[Double]()
  
  def insert(x: Double) {
    buffer += x
  }
  
  def getQuantile(phi: Double): Double = {
    // http://en.wikipedia.org/wiki/Quantile
    val sorted = buffer.sorted
    //val rank1 = math.ceil (phi * buffer.length    ).toInt
    //val rank2 = math.floor(phi * buffer.length + 1).toInt
    val rank = (phi * buffer.length).toInt // math.floor(phi * buffer.length).toInt
    if (phi <= 0.0)
      sorted.head
    else if (phi >= 1.0) 
      sorted.last
    else {
      //println(f"averaging elements with index $rank1 $rank2")
      //(sorted(rank1) + sorted(rank2)) / 2
      sorted(rank)
    }
  }
}


object TestStreamQuantiles {
  def main(args: Array[String]) {
    val data = Array.tabulate(100)(_ => scala.util.Random.nextDouble)
    
    val squantDet = new StreamQuantileTrivial
    data.foreach(x => squantDet.insert(x))

    val squantGK = new StreamQuantileGK(0.03)
    data.foreach(x => squantGK.insert(x))

    println("deterministic quantile:       " + squantDet.getQuantile(0.1))
    println("Greenwald-Khanna quantile:    " + squantGK.getQuantile(0.1))
  }
}









