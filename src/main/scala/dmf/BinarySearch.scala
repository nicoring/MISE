package dmf

/**
 * The general assumption for binary search is that the underlying data structure
 * is sorted according to the default = increasing ordering.
 * 
 * With an increasing ordering the following operations are possible:
 * - first >  than x
 * - first >= than x
 * - first == than x
 * - last  == than x
 * - last  <= than x
 * - last  <  than x
 * 
 * Note that is makes no sense to use binary search for
 * - first < than x (or also <=)
 * - last  > than x (or also >=)
 * because both can be obtained in O(1) by looking at the first/last element any checking whether the condition is fulfilled.
 */
object BinarySearch {

  // http://xusword.blogspot.de/2012/12/the-thing-about-binary-search-part-1.html
  // [1] http://stackoverflow.com/questions/4948162/how-can-i-better-understand-the-one-comparison-per-iteration-binary-search/4948164#4948164

  /*
  def firstElementLargerThan[T](a: Array[T], x: T)(implicit ord: Ordering[T]) {
    
  }*/
 
  
  
  def firstIndex_>[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): Int = {
    implFirst_>(length, i => ord.compare(getter(i), element)) // in [1] arr[testpos] is in the left-hand-side of the comparison 
  }
  def firstIndex_>=[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): Int = {
    implFirst_>=(length, i => ord.compare(getter(i), element)) 
  }
  def firstIndex_==[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): Int = {
    implFirst_==(length, i => ord.compare(getter(i), element)) 
  }
  def lastIndex_==[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): Int = {
    implLast_==(length, i => ord.compare(getter(i), element)) 
  }
  def lastIndex_<=[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): Int = {
    implLast_<=(length, i => ord.compare(getter(i), element)) 
  }
  def lastIndex_<[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): Int = {
    implLast_<(length, i => ord.compare(getter(i), element)) 
  }
  
  def firstElementUnsecure_>[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): T = {
    val foundIndex = implFirst_>(length, i => ord.compare(getter(i), element))
    getter(foundIndex)
  }
  def firstElementUnsecure_>=[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): T = {
    val foundIndex = implFirst_>=(length, i => ord.compare(getter(i), element))
    getter(foundIndex)
  }
  def firstElementUnsecure_==[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): T = {
    val foundIndex = implFirst_==(length, i => ord.compare(getter(i), element))
    getter(foundIndex)
  }
  def lastElementUnsecure_==[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): T = {
    val foundIndex = implLast_==(length, i => ord.compare(getter(i), element))
    getter(foundIndex)
  }
  def lastElementUnsecure_<=[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): T = {
    val foundIndex = implLast_<=(length, i => ord.compare(getter(i), element))
    getter(foundIndex)
  }
  def lastElementUnsecure_<[T](length: Int, element: T, getter: Int => T)(implicit ord: Ordering[T]): T = {
    val foundIndex = implLast_<(length, i => ord.compare(getter(i), element))
    getter(foundIndex)
  }
  
  /**
   * first ... implementations:
   */
  def implFirst_>(length: Int, compare: Int => Int): Int = {
    var lower = 0                   // lower: inclusive
    var upper = length              // upper: exclusive
    while (lower < upper) {
      val testpos = (lower + upper) >>> 1
      val cmp = compare(testpos)
      if (cmp > 0) {                // ">" means "> 0"
        upper = testpos
      }
      else {
        lower = testpos + 1
      }
    }
    return if (lower == length) -1 else lower
  }
  def implFirst_>=(length: Int, compare: Int => Int): Int = {
    var lower = 0
    var upper = length
    while (lower < upper) {
      val testpos = (lower + upper) >>> 1
      val cmp = compare(testpos)
      if (cmp >= 0) {
        upper = testpos
      }
      else {
        lower = testpos + 1
      }
    }
    return if (lower == length) -1 else lower
  }
  def implFirst_==(length: Int, compare: Int => Int): Int = {
    var lower = 0
    var upper = length
    while (lower < upper) {
      val testpos = (lower + upper) >>> 1
      val cmp = compare(testpos)
      if (cmp >= 0) {
        upper = testpos
      }
      else {
        lower = testpos + 1
      }
    }
    val indexFound = if (lower == length) -1 else lower
    return if (indexFound == -1) -1 else if (compare(indexFound)==0) indexFound else -1
  }  
  
  /**
   * last ... implementations:
   */
  def implLast_==(length: Int, compare: Int => Int): Int = {
    var lower = 0
    var upper = length
    while (lower < upper) {
      val testpos = (lower + upper) >>> 1
      val cmp = compare(testpos)
      if (cmp > 0) {        // position (last key <= goal) = position (first key > goal ) - 1       
        upper = testpos
      }
      else {
        lower = testpos + 1
      }
    }
    val indexFound = lower-1
    return if (indexFound == -1) -1 else if (compare(indexFound)==0) indexFound else -1
  }   
  def implLast_<=(length: Int, compare: Int => Int): Int = {
    var lower = 0
    var upper = length
    while (lower < upper) {
      val testpos = (lower + upper) >>> 1
      val cmp = compare(testpos)
      if (cmp > 0) {        // position (last key <= goal) = position (first key > goal ) - 1
        upper = testpos
      }
      else {
        lower = testpos + 1
      }
    }
    return lower-1      
    // for 'last' variants there is no necessity for a check "not found" corresponds to 0 instead of length, subtraction directly corresponds to not-found-semantic
  }   
  def implLast_<(length: Int, compare: Int => Int): Int = {
    var lower = 0
    var upper = length
    while (lower < upper) {
      val testpos = (lower + upper) >>> 1
      val cmp = compare(testpos)
      if (cmp >= 0) {        // position (last key < goal) = position (first key >= goal) - 1
        upper = testpos
      }
      else {
        lower = testpos + 1
      }
    }
    return lower-1      
    // for 'last' variants there is no necessity for a check "not found" corresponds to 0 instead of length, subtraction directly corresponds to not-found-semantic
  }    
  
  
}














