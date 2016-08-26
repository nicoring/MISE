package dmf

import org.scalatest.{FunSpec, Matchers}
import dmf.Implicits._


class TestNormalizableArray extends FunSpec with Matchers {

  val numTests = 10
  def randomArray() = Array.fill(100)(scala.util.Random.nextDouble())

  describe("  NormalizableArray") {
    
    it("should provide sum normalization") {
      for (i <- 0 until numTests) {
        val arr = randomArray()
        arr.normalizedSumOne.sum should be (1.0 +- 0.001)
      }
    }
    
    it("should provide mean normalization") {
      for (i <- 0 until numTests) {
        val arr = randomArray()
        arr.normalizedMeanOne.mean should be (1.0 +- 0.001)
      }
    }
  
    it("should provide zero mean / unit variance normalization") {
      for (i <- 0 until numTests) {
        val arr = randomArray()
        val narr = arr.normalizedUnitVariance
        narr.mean should be (0.0 +- 0.001)
        narr.vari should be (1.0 +- 0.001)
        narr.sdev should be (1.0 +- 0.001)
      }
    }

  }
}