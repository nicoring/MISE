package dmf.stream

import Types._
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.GenIterable
import scala.collection.IterableLike
import scala.collection.TraversableLike


object Types {
  type StreamData = Array[Double] 
}

case class StreamEntry(time: Long, data: StreamData, info: GroundTruth = BooleanGroundTruth(false))


sealed trait GroundTruth extends Any {
  def isChange(): Boolean
}
case class BooleanGroundTruth(val underlying: Boolean) extends AnyVal with GroundTruth {
  def isChange() = underlying
}

class BooleanGroundTruthChangeDetector{
  var lastState = -999d
  def apply(state: Double): BooleanGroundTruth = {
    val change = (state != lastState)
    lastState = state
    BooleanGroundTruth(change)
  } 
}



trait Stream extends Iterator[StreamEntry] {
  self =>
  
  val knownLength: Option[Long] // = None
  val dim: Int
  val attrNames: Array[String]
  
  // abstract iterator, delegate hasNext and next to this abstract iterator
  val iterator: Iterator[StreamEntry]
  def hasNext = iterator.hasNext
  def next()  = iterator.next()
  
  // override a few Iterator members to return a Stream instead of Iterator[StreamEntry]
  override def take(n: Int): Stream = new Stream {
    val knownLength   = Some(n.toLong)
    val dim           = self.dim
    val attrNames     = self.attrNames
    val iterator      = self.iterator.take(n)
  }
  /** Additional 'take' with Option argument
   */
  def takeOption(n: Option[Int]): Stream = n match {
    case Some(n) => self.take(n)
    case None    => self
  }
  override def drop(n: Int): Stream = new Stream {
    val knownLength   = self.knownLength.map(_ - n)
    val dim           = self.dim
    val attrNames     = self.attrNames
    val iterator      = self.iterator.drop(n)
  }
  override def slice(from: Int, until: Int): Stream = new Stream {
    val knownLength   = Some((until-from).toLong)
    val dim           = self.dim
    val attrNames     = self.attrNames
    val iterator      = self.iterator.slice(from, until)
  }
  
  // special version of map to return a Stream
  def map(f: StreamEntry => StreamEntry): Stream = new Stream {
    val knownLength   = self.knownLength
    val dim           = self.dim
    val attrNames     = self.attrNames
    val iterator      = self.iterator.map(f)
  }
  
  def attributesReduce(attrToKeep: Array[Int]): Stream = {
    assert(attrToKeep.max < dim)
    new Stream {
      val knownLength   = self.knownLength
      val dim           = attrToKeep.size
      val attrNames     = attrToKeep.map(i => self.attrNames(i))
      val iterator      = self.iterator.map{ entry => 
        entry.copy(data = attrToKeep.map(i => entry.data(i)))
      }
    }    
  }
  
  def attributesAdd(variable: Iterator[Double], varname: String): Stream = {
    new Stream {
      val knownLength   = self.knownLength
      val attrNames     = self.attrNames :+ varname
      val dim           = attrNames.length
      val iterator      = self.iterator.map{ entry => 
        entry.copy(data = entry.data :+ variable.next())
      }
    }    
  }
}



case class BasicStream(
    dim: Int, 
    iterator: Iterator[StreamEntry],
    knownLength: Option[Long] = None 
  ) extends AbstractStream {
  val attrNames = Array.tabulate(dim)(j => f"A$j%03d")
} 

case class BasicStreamNamedAttributes(
    attrNames: Array[String], 
    iterator: Iterator[StreamEntry],
    knownLength: Option[Long] = None 
  ) extends AbstractStream {
  val dim = attrNames.length
} 

/** Explicit instantiation of the `Stream` trait to reduce class file size in subclasses. */
private[stream] abstract class AbstractStream extends Stream


/*
trait Stream extends TraversableLike[StreamEntry, Stream] with IterableLike[StreamEntry, Stream] 
  // extends Traversable[StreamEntry] with TraversableLike[StreamEntry, Stream] with IterableLike[StreamEntry, Stream] 
  // Iterable[StreamEntry] 
  // with GenericTraversableTemplate[StreamEntry, Stream]
  // with GenIterable[StreamEntry] with  GenericTraversableTemplate[StreamEntry, Stream]
  // extends //PeekableIterator[StreamEntry]
{
  val knownLength: Option[Long] = None
  val dim: Int
  val attrNames: Array[String]
  
  def seq = this
  
  def newBuilder: scala.collection.mutable.Builder[StreamEntry, Stream] = ???
  /*
  def limitOptionally(length: Option[Int]): PeekableIterator[StreamEntry] = {
    length match {
      case Some(l) => new PeekableIteratorWrapper(this.take(l))
      case None    => this
    }
  }
  */
  //override def take(n: Int): Stream = new PeekableIteratorWrapper(this.take(n))
}

class BasicStream(val attrNames: Array[String], val iterator: Iterator[StreamEntry]) extends Stream {
  val dim = attrNames.length
}
*/