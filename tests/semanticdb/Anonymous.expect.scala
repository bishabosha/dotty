package example
import scala.language/*=>>scalaShadowing.language.*/.higherKinds/*=>>scalaShadowing.language.higherKinds.*/

class Anonymous/*<<=example.Anonymous#*/ {
  this: Anonymous/*=>>example.Anonymous#*/ =>

  def locally/*<<=example.Anonymous#locally().*/[A/*<<=example.Anonymous#locally().[A]*/](x/*<<=example.Anonymous#locally().(x)*/: A/*=>>example.Anonymous#locally().[A]*/): A/*=>>example.Anonymous#locally().[A]*/ = x/*=>>example.Anonymous#locally().(x)*/

  def m1/*<<=example.Anonymous#m1().*/[T/*<<=example.Anonymous#m1().[T]*/[_]] = ???/*=>>scala.Predef.`???`().*/
  def m2/*<<=example.Anonymous#m2().*/: Map/*=>>scala.Predef.Map#*/[_, List/*=>>scala.List#*/[_]] = ???/*=>>scala.Predef.`???`().*/
  locally/*=>>example.Anonymous#locally().*/ {
    ???/*=>>scala.Predef.`???`().*/ match { case _: List/*=>>scala.List#*/[_] => }
  }
  locally/*=>>example.Anonymous#locally().*/ {
    val x/*<<=local0*/: Int/*=>>scala.Int#*/ => Int/*=>>scala.Int#*/ = _ => ???/*=>>scala.Predef.`???`().*/
  }

  trait Foo/*<<=example.Anonymous#Foo#*/
  new Foo/*=>>example.Anonymous#Foo#*/ {}
}
