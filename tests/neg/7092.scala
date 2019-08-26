import scala.annotation.implicitNotFound

@implicitNotFound("Not found for ${A}")
type F[A]

@implicitNotFound("Not found for ${A}")
trait G[A]

@implicitNotFound("Not found for ${A}, ${B}")
type H = [A] =>> [B] =>> (A, B)

def f[T] given F[T] = ???
def g[T] given G[T] = ???
// def h[T] given H[T][?] = ???

val errors = {
  f[String] // error
  g[String] // error
  the[H[Int]] // error
}