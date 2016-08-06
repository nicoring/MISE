package dmf


import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import dmf._

class TestSortedBuffers extends FunSpec with ShouldMatchers {

  describe("  a sorted buffer") {

    /*
    val buffer = new SortedBuffer[Double](5)(new Ordering[Double]{
      def compare(a: Double, b: Double) = scala.math.Ordering.Double.compare(a, b)
    })
    */

    it("should work on doubles (keep low values)") {
      
      val buffer = new SortedBufferKeepLow[Double](3)
      buffer.add(2.0)
      buffer.add(1.0)
      buffer.add(3.0)
      buffer.iterator.toList.sorted should equal (List(1.0, 2.0, 3.0))
      buffer.add(5.0)
      buffer.add(4.0)
      buffer.iterator.toList.sorted should equal (List(1.0, 2.0, 3.0))
      buffer.min should equal (1.0)
      buffer.max should equal (3.0)
      // adding more while at limit
      buffer.add(5.0) should equal (false)
      buffer.add(4.0) should equal (false)
      buffer.add(0.0) should equal (true)
      buffer.add(4.0) should equal (false)
    }

    it("should work on doubles (keep high values)") {
      
      val buffer = new SortedBufferKeepHigh[Double](3)
      buffer.add(2.0)
      buffer.add(1.0)
      buffer.add(3.0)
      buffer.iterator.toList.sorted should equal (List(1.0, 2.0, 3.0))
      buffer.add(5.0)
      buffer.add(4.0)
      buffer.iterator.toList.sorted should equal (List(3.0, 4.0, 5.0))
      buffer.min should equal (3.0)
      buffer.max should equal (5.0)
      // adding more while at limit
      buffer.add(1.0) should equal (false)
      buffer.add(2.0) should equal (false)
      buffer.add(6.0) should equal (true)
      buffer.add(2.0) should equal (false)
    }

    it("should work for nearest neighbors") {
      val buffer = new NNBuffer(3)
      // ensure the comparison function works correctly
      Neighbor(1, 1.0) should equal (Neighbor(1, 1.0))
      // fill the buffer
      buffer.add(new Neighbor(1, 1.0)) should equal (true)
      buffer.add(new Neighbor(1, 2.0)) should equal (true)
      buffer.add(new Neighbor(1, 3.0)) should equal (true)
      buffer.max should equal (new Neighbor(1, 3.0))
      buffer.add(new Neighbor(1, 4.0)) should equal (false)
      buffer.max should equal (new Neighbor(1, 3.0))
      buffer.add(new Neighbor(1, 2.5)) should equal (true)
      buffer.max should equal (new Neighbor(1, 2.5))
      buffer.add(new Neighbor(1, 1.0)) should equal (true)
      buffer.max should equal (new Neighbor(1, 2.0))
      buffer.add(new Neighbor(1, 5.0)) should equal (false)
      buffer.max should equal (new Neighbor(1, 2.0))
      buffer.add(new Neighbor(1, 1.0)) should equal (true)
      buffer.max should equal (new Neighbor(1, 1.0))
      buffer.min should equal (new Neighbor(1, 1.0))
      buffer.add(new Neighbor(1, 1.0)) should equal (false) // Note: if the added element is the same as the last object, the output will be a reject, although it is also possible to consider it the other way around 
      buffer.add(new Neighbor(1, 0.5)) should equal (true)
      buffer.add(new Neighbor(1, 0.3)) should equal (true)
      buffer.add(new Neighbor(1, 0.8)) should equal (true)
      buffer.max should equal (new Neighbor(1, 0.8))
      buffer.min should equal (new Neighbor(1, 0.3))
    }
    
  }
  
}




    
    
    
    
    
    
    
    