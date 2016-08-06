package dmf.stream.mutinf

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random

class TestNeighbor2D extends FunSpec with ShouldMatchers {

  describe("Neighbor2D"){
    
    it("should provide correct results for points on a line") {
      val dataX = Array(0d, 0d, 0d, 0d, 0d)
      val dataY = Array(0d, 1d, 3d, 7d, 9d)
      val knn = new Neighbor2DFactory(dataX, dataY)
      /*
      println(knn.getNeighborhoodOf(0, 2))
      println(knn.getNeighborhoodOf(1, 2))
      println(knn.getNeighborhoodOf(2, 2))
      println(knn.getNeighborhoodOf(3, 2))
      println(knn.getNeighborhoodOf(4, 2))
      */
      knn.getNeighborhoodOf(0, 1).last.ind should equal (1)
      knn.getNeighborhoodOf(1, 1).last.ind should equal (0)
      knn.getNeighborhoodOf(2, 1).last.ind should equal (1)
      knn.getNeighborhoodOf(3, 1).last.ind should equal (4)
      knn.getNeighborhoodOf(4, 1).last.ind should equal (3)

      knn.getNeighborhoodOf(0, 2).last.ind should equal (2)
      knn.getNeighborhoodOf(1, 2).last.ind should equal (2)
      knn.getNeighborhoodOf(2, 2).last.ind should equal (0)
      knn.getNeighborhoodOf(3, 2).last.ind should equal (2)
      knn.getNeighborhoodOf(4, 2).last.ind should equal (2)
    }
    
    it("should provide correct results for other examples") {
      val dataX = Array(3d, 1d, 0d, 0d)
      val dataY = Array(0d, 1d, 0d, 5d)
      val knn = new Neighbor2DFactory(dataX, dataY)

      knn.getNeighborhoodOf(0, 1).last.ind should equal (1)
      knn.getNeighborhoodOf(1, 1).last.ind should equal (2)
      knn.getNeighborhoodOf(2, 1).last.ind should equal (1)
      knn.getNeighborhoodOf(3, 1).last.ind should equal (1)
    }

    it("should provide correct results on large uniform example") {
      val N = 100
      val ks = Seq(10, 50)
      val numChecks = 20
      for (iteration <- Range(0, 10)) {
        val dataX = Array.tabulate(N)( _ => Random.nextDouble())
        val dataY = Array.tabulate(N)( _ => Random.nextDouble())
        val knn = new Neighbor2DFactory(dataX, dataY)
        
        for (check <- Range(0, numChecks)) {
          val i = Random.nextInt(N)
          val distances = Range(0, N).map{ j => (math.abs(dataX(i)-dataX(j)) max math.abs(dataY(i)-dataY(j)), j) }
          val distancesSorted = distances.sorted
          for (k <- ks) {
            val neighbors = knn.getNeighborhoodOf(i, k)
            val neighbor = neighbors.last
            //println(distancesSorted.take(k+1))
            //println(neighbors)
            neighbor.distance should equal (distancesSorted(k)._1)
          }
        }
      }

    }
    
  }
}