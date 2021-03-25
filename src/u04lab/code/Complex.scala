package u04lab.code

trait Complex {
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}
/*
object Complex {
  def apply(re:Double, im:Double):Complex = new ComplexImpl(re, im) // Fill here

  private class ComplexImpl( override val re: Double,
                             override val im: Double) extends Complex{
    override def +(c: Complex): Complex = apply(c.re + this.re, c.im + this.im)

    override def *(c: Complex): Complex = apply((this.re * c.re)-(this.im * c.im), (this.re * c.im) + (this.im * c.re))
  }
}*/

case class ComplexImpl( override val re: Double,
                        override val im: Double) extends Complex{
  override def +(c: Complex): Complex = ComplexImpl(c.re + this.re, c.im + this.im)

  override def *(c: Complex): Complex = ComplexImpl((this.re * c.re)-(this.im * c.im), (this.re * c.im) + (this.im * c.re))
}

object TryComplex extends App {
  val a = Array(ComplexImpl(10,20), ComplexImpl(1,1), ComplexImpl(7,0))
  val c = a(0) + a(1) + a(2)
  println(c, c.re, c.im) // (ComplexImpl(18.0,21.0),18.0,21.0)
  val c2 = a(0) * a(1)
  println(c2, c2.re, c2.im) // (ComplexImpl(-10.0,30.0),-10.0,30.0)
}

/** Hints:
  * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  * - check that equality and toString do not work
  * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  * - check equality and toString now
  */