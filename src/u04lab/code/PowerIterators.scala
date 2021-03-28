package u04lab.code

import Optionals._
import Lists._
import Streams._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A])
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

object StreamUtils {
  def toStream[A](list: List[A]): Stream[A]= list match {
    case List.Cons(h, t) => Stream.Cons(() => h, () => toStream(t))
    case List.Nil() => Stream.empty()
  }
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  case class PowerIteratorImpl[A](private var stream: Stream[A]) extends PowerIterator[A]{
    private var pastList: List[A] = List.nil

    override def next(): Option[A] = stream match {
      case Stream.Cons(h, t) => {
        stream = t()
        pastList = List.Cons(h(), pastList)
        Option.of(h())
      }
    }

    override def allSoFar(): List[A] = List.reverse(pastList)

    override def reversed(): PowerIterator[A] = PowerIteratorImpl(StreamUtils.toStream(List.reverse(pastList)))
  }

  override def incremental(start: Int, successive: Int => Int):  PowerIterator[Int] = PowerIteratorImpl(Stream.iterate(start)(successive))

  override def fromList[A](list: List[A]): Unit = StreamUtils.toStream(list)

  override def randomBooleans(size: Int): PowerIterator[Boolean] = PowerIteratorImpl(Stream.take(Stream.generate(Random.nextBoolean()))(size))

}
