package dmf.stream.generators

import dmf.stream._
import dmf.stream.Types._



object SynthFuncDep {

  def inInterval(x: Double, l: Double, r: Double) = (x > l) && (x < r)
  
  class CustomStreamEntryIterator(dim: Int) extends Iterator[StreamEntry] {
    var i = 0l
    def hasNext = true
    def next(): StreamEntry = {
      
      val data = Array.tabulate(dim)(j => scala.util.Random.nextDouble)
      if (/*inInterval(data(0), 0.1, 0.2) &&*/ inInterval(data(1), 0.2, 0.4) && inInterval(data(2), 0.3, 0.6)) {
        data(3) = 2*data(1) + 10*data(2)
      }
      
      val entry = StreamEntry(i, data, BooleanGroundTruth(false))
      i += 1
      entry
    }
    
  }
  
  def getStream(dim: Int): Stream = {
    BasicStream(dim, new CustomStreamEntryIterator(dim))
  }
 

}