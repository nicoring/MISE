package dmf

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import dmf._

class TestHelpers extends FunSpec with ShouldMatchers {

  val verbose = false
  val logging = Helpers.outputDummy
  
  describe("  the helper functions") {
    it("should provide a power set function") {
      Helpers.powerSet(Set()) should equal (Set(Set()))
      Helpers.powerSet(Set(1)) should equal (Set(Set(), Set(1)))
      Helpers.powerSet(Set(1,2)) should equal (Set(Set(), Set(1), Set(2), Set(1,2)))
      Helpers.powerSet(Set(1,2,3)) should equal (Set(Set(), Set(1), Set(2), Set(3), Set(1,2), Set(2,3), Set(1,3), Set(1,2,3)))
      Helpers.powerSet(Set("1","2")) should equal (Set(Set(), Set("1"), Set("2"), Set("1","2")))
    }
    
    it("should provide a conversion: partion => bitsets") {
      import collection.mutable.BitSet
      Helpers.convertPartitionToBitSets(Seq(1,1,1)) should equal (Seq(BitSet(0), BitSet(1), BitSet(2)))
      Helpers.convertPartitionToBitSets(Seq(2,3)) should equal (Seq(BitSet(0,1), BitSet(2,3,4)))
      Helpers.convertPartitionToBitSets(Seq(3,2)) should equal (Seq(BitSet(0,1,2), BitSet(3,4)))
      Helpers.convertPartitionToBitSets(Seq()) should equal (Seq())
      Helpers.convertPartitionToBitSets(Seq(5)) should equal (Seq(BitSet(0,1,2,3,4)))
    }
  }
  
  describe("  the precalcSortIndices") {

    it("should provide correct result for a basic example") {
      
      val m = Matrix.createDoubleMatrix(List(
          List(40, 51),
          List(42, 36),
          List(30, 33),
          List(20, 30),
          List(60, 31)
          ))
      
      val indexM = Helpers.precalcSortIndices(m)
     
      indexM.numRows should equal(m.numRows)
      indexM.numCols should equal(m.numCols)
      indexM(0,0) should equal (3)
      indexM(1,0) should equal (2)
      indexM(2,0) should equal (0)
      indexM(3,0) should equal (1)
      indexM(4,0) should equal (4)
      indexM(0,1) should equal (3)
      indexM(1,1) should equal (4)
      indexM(2,1) should equal (2)
      indexM(3,1) should equal (1)
      indexM(4,1) should equal (0)
      //println(indexM)
    }
  }
  
  describe("  the iterative mean/variance calucation") {
  
    it("should be within numerical limits to a two pass calculation") {
      val meanvar = new IterativeMeanAndVariance()
      val seq = Seq(1.0, 1.4, 2.1, 0.4, 2.3, 4.2, 1.4, 1.1, 0.9, 5.0, 1.8)
      meanvar.add(seq :_*)
      import dmf.Implicits._
      val precision = 0.00000001
      seq.mean should be (meanvar.mean plusOrMinus precision) 
      seq.vari should be (meanvar.vari plusOrMinus precision)
    }
  }
  
}



    
    
    
    
    
    
    
    