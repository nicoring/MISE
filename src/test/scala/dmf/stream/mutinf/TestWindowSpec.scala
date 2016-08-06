package dmf.stream.mutinf

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random



class TestWindowSpec extends FunSpec with ShouldMatchers {

  def reference(nD: Int, nT: Int) = Iterator.iterate(0L)(_+1L)
                                            .drop(nD)
                                            .take(nT)
                                            .toSet
  def w(nD: Int, nT: Int) = new WindowSpec(nD, nT)
                                            
  describe("WindowSpec"){
    
    it("should correspond drop/take semantics: same indices") {
      for (nD <- Range(0, 10)) {
        for (nT <- Range(0, 10)) {
          w(nD, nT).indices.toSet should equal (reference(nD, nT))
        }
      }
    }
    it("should correspond drop/take semantics: correct size") {
      for (nD <- Range(0, 10)) {
        for (nT <- Range(0, 10)) {
          w(nD, nT).size should equal (nT)
          w(nD, nT).size should equal (w(nD, nT).indices.length)
        }
      }
    }
    it("should correspond drop/take semantics: correct min/max") {
      for (nD <- Range(0, 10)) {
        for (nT <- Range(0, 10)) {
          // note semantics of lIncluded and rIncluded are only defined for nT >= 1
          if (nT > 0) {
            w(nD, nT).indexMin should equal (reference(nD, nT).min)
            w(nD, nT).indexMax should equal (reference(nD, nT).max)
          }
        }
      }
    }
    it("should correspond drop/take semantics: index filtering") {
      for (nD <- Range(0, 10)) {
        for (nT <- Range(0, 10)) {
          val indices = Range(-100, 100)
          val filteredByR = indices.filter(i => reference(nD, nT).contains(i))
          val filteredByW = indices.filter(i => w(nD, nT).indexFilter(i))
          filteredByW should equal (filteredByR)
          filteredByW.toSet should equal (w(nD, nT).indices.toSet)
        }
      }
    }
    it("should provide construction from index semantics") {
      for (indL <- Range(-100, 100)) {
        for (indR <- Range(indL, 100)) {
          val w = WindowSpec.createFromIndices(indL, indR)
          w.indexMin should equal (indL)
          w.indexMax should equal (indR)
        }
      }
    }
    it("should provide reversed construction") {
      for (nD <- Range(0, 10)) {
        for (nT <- Range(0, 10)) {
          for (sl <- Range(nD+nT, 100)) {
            val w = WindowSpec.createReverse(sl, nD, nT)
            val reference = Iterator.iterate(0L)(_+1L).take(sl).toList.reverse.drop(nD).take(nT).reverse // note reverse again in the end, since indices should still be INCREASING
            w.indices.toList should equal (reference)
          }
        }
      }
    }  
  
  }
  
}











