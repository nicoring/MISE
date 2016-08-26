package dmf.stream

import org.scalatest.{FunSpec, Matchers}

import scala.language.postfixOps


class TestStream extends FunSpec with Matchers {

  def genTestStream() = new Stream {
    val knownLength = None
    val attrNames = Array("A", "B", "C")
    val dim = attrNames.length
    val iterator = 
      StreamEntry( 0, Array( 1.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 1, Array( 2.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 2, Array( 3.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 3, Array( 4.0, 7.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 4, Array( 5.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 5, Array( 6.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 6, Array( 7.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 7, Array( 8.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 8, Array( 9.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      StreamEntry( 9, Array(10.0, 1.0, 1.0), BooleanGroundTruth(false)) ::
      Nil toIterator
    
  }
  
  describe("The Stream trait"){
    
    it("should be convertible to other collections") {
      val sarr  = genTestStream().toArray
      val slst  = genTestStream().toList
      val sitrb = genTestStream().toIterable
      val sitrt = genTestStream().toIterator
      sarr.length should be (10)
      slst.length should be (10)
      sitrb.count(_ => true) should be (10)
      sitrt.length should be (10)
      sarr(3).data(0) should be (4.0 +- 0.01)
    }
    
    it("should provide 'take'") {
      val stream = genTestStream()
      stream.take(3)
      stream.take(3).toArray.length should equal (3)
      stream.toArray.length should equal (7)
    }
    
    it("should be convertible to BufferedIterator") {
      // length invariance check
      genTestStream().buffered.length should be (10)
      genTestStream().buffered.buffered.length should be (10)
      genTestStream().buffered.buffered.toIterator.buffered.length should be (10)
      genTestStream().take(10).buffered.length should be (10)
      // content invariance check
      genTestStream().buffered.toArray.apply(3).data(0) should be (4.0 +- 0.01)
      genTestStream().buffered.buffered.toArray.apply(3).data(0) should be (4.0 +- 0.01)
      genTestStream().buffered.buffered.toIterator.buffered.toArray.apply(3).data(0) should be (4.0 +- 0.01)
      genTestStream().take(10).buffered.toArray.apply(3).data(0) should be (4.0 +- 0.01)
    }
    
    it("should allow subspace projections") {
      genTestStream().attributesReduce(Array(1)).length should be (10)
      genTestStream().attributesReduce(Array(1)).dim should be (1)
      genTestStream().attributesReduce(Array(1)).attrNames.length should be (1)
      genTestStream().attributesReduce(Array(1)).toArray.apply(3).data.length should be (1)
      genTestStream().attributesReduce(Array(1)).toArray.apply(3).data(0) should be (7.0 +- 0.01)
    }      
  
  }
  
}