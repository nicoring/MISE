package dmf.stream.quantiles


case class SumTuple(v: Double, g: Int, delta: Int) extends Ordered[SumTuple] {
  def compare(that: SumTuple) = Ordering.Double.compare(this.v, that.v)
  override def toString = f"($v%.2f, $g, $delta)"
}


class StreamQuantileGK(val epsilon: Double) extends StreamQuantile {
  val buffer = new java.util.LinkedList[SumTuple]()
  var n = 0
  
  def insert(x: Double) {
    if (buffer.isEmpty()) {
      buffer.addFirst(SumTuple(x, 1, 0))
    }
    else if (x < buffer.getFirst().v) {
      buffer.addFirst(SumTuple(x, 1, 0))
    }    
    else if (x > buffer.getLast().v) {
      buffer.addLast(SumTuple(x, 1, 0))
    }
    else {
      var index = 0
      val i   = buffer.iterator()
      val ip1 = buffer.iterator()
      ip1.next()  // skip one element, this is not critical since we know that the buffer has at least two elements here
      var found = false
      while (!found) {
        val vi   = i.next().v
        val vip1 = ip1.next().v
        if (vi < x && x <= vip1) {
          found = true
          buffer.add(index+1, SumTuple(x, 1, math.floor(2.0*epsilon*n).toInt))
        }
        else {
          index += 1
        }
      }
    }
    n += 1
    compress()
    println(f"2εn = ${2.0*epsilon*n}%.3f    inserted = $x%.3f     buffer = $buffer")
  }
  
  def compress() {
    if (buffer.size() >= 2) {
      for (i <- Range(0, buffer.size()-1).reverse) {
        val ti   = buffer.get(i)
        val tip1 = buffer.get(i+1)
        if (ti.g + tip1.g + tip1.delta <= 2.0*epsilon*n) {
          buffer.remove(i+1)
          buffer.remove(i)
          buffer.add(i, SumTuple(tip1.v, ti.g+tip1.g, tip1.delta))
          println(f"merging $i with ${i+1}")
        }
      }
    }
  }
  
  def getQuantile(phi: Double): Double = {
    var ri = 0.0
    val it = buffer.iterator()
    var found = false
    var previous = buffer.getFirst()
    while (it.hasNext && !found) {
      val ti = it.next()
      println(ri, ti.g, ti.delta, (phi*n + epsilon*n))
      if (ri + ti.g + ti.delta > (phi*n + epsilon*n)) {
        found = true
      } else {
        previous = ti
        ri += ti.g
      }
    }
    previous.v
  }
}



