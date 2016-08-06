package dmf.stream.generators

import dmf.stream._
import dmf.stream.Types._
import scala.util.Random
import org.apache.commons.math3.distribution.MultivariateNormalDistribution
import org.apache.commons.math3.linear.MatrixUtils



object SynthGaussianMixture {

  def getStream(changeProb: Double, initComps: Int = 5, maxComps: Int = 10, minComps: Int = 1): Stream = {
    
    val iterator = new Iterator[StreamEntry] {
      
      def createNewComponent() = {
        val corr = Random.nextDouble
        val corrMat = MatrixUtils.createRealMatrix(Array(Array(1.0, corr), Array(corr, 1.0)))
        val diagMat = MatrixUtils.createRealMatrix(Array(Array(Random.nextDouble, 0.0), Array(0.0, Random.nextDouble)))
        val covMat = diagMat multiply corrMat multiply diagMat
        new MultivariateNormalDistribution(Array(Random.nextDouble, Random.nextDouble), covMat.getData())        
      }

      var i = 0l
      val components = collection.mutable.Set[MultivariateNormalDistribution]()
      Range(0, initComps).foreach(_ => components += createNewComponent())
      
      def hasNext = true
      def next(): StreamEntry = {
        
        // add?
        if (Random.nextDouble < changeProb && components.size < maxComps) {
          components += createNewComponent()
        }
        // delete?
        if (Random.nextDouble < changeProb && components.size > minComps) {
          val randElem = Random.shuffle(components.toList).head
          components.remove(randElem)
        }
        
        val data = Random.shuffle(components.toList).head.sample()
        val entry = StreamEntry(i, data)

        i += 1
        entry
      }
    }
    
    BasicStream(2, iterator)
  }
 
}







