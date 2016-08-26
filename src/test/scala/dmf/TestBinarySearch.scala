package dmf

import org.scalatest.{FunSpec, Matchers}

import scala.util.Random
//import scala.Ordering


class TestBinarySearch extends FunSpec with Matchers {

  val iterations = 10
  val valueRange = 3
  val arrMinLen = 0
  val arrMaxLen = 10
  
  
  describe("Binnary Search") {

    /**
     * Test for first ... implementations
     */
    it("should provide a 'first >' implementation") {
      for (iter <- Range(0, iterations)) {
        for (arrLen <- Range(arrMinLen, arrMaxLen+1)) {
          val randArr = Array.tabulate(arrLen)(_ => Random.nextInt(valueRange)).sorted
          for (x <- Range(0, valueRange)) {
            val correctIndex = randArr.indexWhere(_ > x)    // note: inverse is lastIndexWhere
            val binsrchIndex = BinarySearch.firstIndex_>(randArr.length, x, randArr.apply _)
            // println(x, randArr.mkString("[", ", ", "]"), correctIndex, binsrchIndex)
            binsrchIndex should equal (correctIndex)
          }
        }
      }
    }
    it("should provide a 'first >=' implementation") {
      for (iter <- Range(0, iterations)) {
        for (arrLen <- Range(arrMinLen, arrMaxLen+1)) {
          val randArr = Array.tabulate(arrLen)(_ => Random.nextInt(valueRange)).sorted
          for (x <- Range(0, valueRange)) {
            val correctIndex = randArr.indexWhere(_ >= x)
            val binsrchIndex = BinarySearch.firstIndex_>=(randArr.length, x, randArr.apply _)
            binsrchIndex should equal (correctIndex)
          }
        }
      }
    }
    it("should provide a 'first ==' implementation") {
      for (iter <- Range(0, iterations)) {
        for (arrLen <- Range(arrMinLen, arrMaxLen+1)) {
          val randArr = Array.tabulate(arrLen)(_ => Random.nextInt(valueRange)).sorted
          for (x <- Range(0, valueRange)) {
            val correctIndex = randArr.indexWhere(_ == x)
            val binsrchIndex = BinarySearch.firstIndex_==(randArr.length, x, randArr.apply _)
            binsrchIndex should equal (correctIndex)
          }
        }
      }
    }
    
    /**
     * Test for last ... implementations
     */
    it("should provide a 'last ==' implementation") {
      for (iter <- Range(0, iterations)) {
        for (arrLen <- Range(arrMinLen, arrMaxLen+1)) {
          val randArr = Array.tabulate(arrLen)(_ => Random.nextInt(valueRange)).sorted
          for (x <- Range(0, valueRange)) {
            val correctIndex = randArr.lastIndexWhere(_ == x)
            val binsrchIndex = BinarySearch.lastIndex_==(randArr.length, x, randArr.apply _)
            binsrchIndex should equal (correctIndex)
          }
        }
      }
    }  
    it("should provide a 'last <=' implementation") {
      for (iter <- Range(0, iterations)) {
        for (arrLen <- Range(arrMinLen, arrMaxLen+1)) {
          val randArr = Array.tabulate(arrLen)(_ => Random.nextInt(valueRange)).sorted
          for (x <- Range(0, valueRange)) {
            val correctIndex = randArr.lastIndexWhere(_ <= x)
            val binsrchIndex = BinarySearch.lastIndex_<=(randArr.length, x, randArr.apply _)
            binsrchIndex should equal (correctIndex)
          }
        }
      }
    }
    it("should provide a 'last <' implementation") {
      for (iter <- Range(0, iterations)) {
        for (arrLen <- Range(arrMinLen, arrMaxLen+1)) {
          val randArr = Array.tabulate(arrLen)(_ => Random.nextInt(valueRange)).sorted
          for (x <- Range(0, valueRange)) {
            val correctIndex = randArr.lastIndexWhere(_ < x)
            val binsrchIndex = BinarySearch.lastIndex_<(randArr.length, x, randArr.apply _)
            binsrchIndex should equal (correctIndex)
          }
        }
      }
    }  
    
  }
}
