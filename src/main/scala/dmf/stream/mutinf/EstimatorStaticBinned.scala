package dmf.stream.mutinf

import dmf.stream.quantiles.StreamQuantileTrivial
import dmf.Helpers


object EstimatorStaticBinned {
  def apply(): EstimatorStaticBinned = {
    val adaptive = Helpers.readUserInput("use adaptive binning = ", false)
    val force = Helpers.readUserInput("force a fixed number of bins = ", false)
    if (force) {
      val numBins = Helpers.readUserInput("numBins = ", 5)
      new EstimatorStaticBinned(Some(numBins), adaptive)
    } else {
      new EstimatorStaticBinned(None, adaptive)
    }
  }
} 


class EstimatorStaticBinned(val forceFixedNumberOfBins: Option[Int] = None, adaptiveBinning: Boolean = false) extends EstimatorStatic {

  val minPossibleQuerySize = 1
  override def toString() = f"BinnedEstimator_${if (adaptiveBinning) "A" else "F"}"
  
  def calcMI(dataX: Array[Double], dataY: Array[Double]): Double = {
    
    val N = dataX.length
    val numBins = forceFixedNumberOfBins match {
      case Some(number) => number
      case _            => math.ceil(math.pow(N, 1.0/3.0) * 2).toInt
      // http://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width
      // this is simply Rice formula to ensure best runtime
      // Struges: math.ceil(math.log(N)/math.log(2) + 1).toInt        (gaussian assumption; grows a bit too slow?)
      // Scott or Friedman-Diaconis might be better in terms of quality but requires additional calculations (variance / inter-quartile-range)
    }
  
    val minXeps = dataX.min - 0.0001 // 100000*Double.MinPositiveValue // TODO: check why does the machine epsilon break the later count assert (i.e. why isn't max-eps < max?)
    val maxXeps = dataX.max + 0.0001 // 100000*Double.MinPositiveValue
    val minYeps = dataY.min - 0.0001 // 100000*Double.MinPositiveValue
    val maxYeps = dataY.max + 0.0001 // 100000*Double.MinPositiveValue
    
    val (positionsX, positionsY) = if (adaptiveBinning) {
      val quantX = new StreamQuantileTrivial()
      val quantY = new StreamQuantileTrivial()
      
      for (i <- 0 until dataX.length) {
        quantX.insert(dataX(i))
        quantY.insert(dataY(i))
      }
      val quantiles = Range(1, numBins).map(i => i.toDouble / numBins)
      (minXeps +: quantiles.map(q => quantX.getQuantile(q)) :+ maxXeps,   // TODO: significant speedup possible by getting all quantiles at once (currently each requires N log N sorting of data)
       minYeps +: quantiles.map(q => quantY.getQuantile(q)) :+ maxYeps)
    } else {
      def range(from: Double, to: Double): IndexedSeq[Double] = Range(0, numBins+1).map(i => from + (to-from)/numBins*i)
      (range(minXeps, maxXeps),
       range(minYeps, maxYeps))
    }
    
    val countX = Array.tabulate(numBins)(i => dataX.filter(x => x>=positionsX(i) && x<positionsX(i+1)).length )
    val countY = Array.tabulate(numBins)(j => dataY.filter(y => y>=positionsY(j) && y<positionsY(j+1)).length )
    val countXY = Array.tabulate(numBins){i => 
      Array.tabulate(numBins){j => 
        dataX.indices.filter{k =>
          dataX(k)>=positionsX(i) && dataX(k)<positionsX(i+1) && dataY(k)>=positionsY(j) && dataY(k)<positionsY(j+1)
        }.length
      }
    }
    val volumes = Array.tabulate(numBins){i => 
      Array.tabulate(numBins){j => 
        (positionsX(i+1) - positionsX(i)) * (positionsY(j+1) - positionsY(j))
      }
    }

    val totalVolume = (maxXeps-minXeps) * (maxYeps-minYeps)
    
    assert(countXY.flatten.sum == N)
    //println(countX mkString "; ")
    //println(countY mkString "; ")
    //println(countXY mkString "; ")
    
    implicit class CountToProb(c: Int) {
      def toProbability = c.toDouble / N
    }
    
    var mi = 0.0
    val log2 = math.log(2)
    for (i <- Range(0, numBins)) {
      for (j <- Range(0, numBins)) {
        val pXY = countXY(i)(j).toProbability
        val pX = countX(i).toProbability
        val pY = countY(j).toProbability
        // println(pXY, pX, pY, pXY * math.log(pXY / (pX*pY))/log2)
        // convention: 0 * log(0) = 0
        if (pXY != 0) {
          mi += pXY * math.log(pXY / (pX*pY)) / log2
        }
      }
    }
    
    mi    
  }
  
}