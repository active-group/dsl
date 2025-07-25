trait Functor[F[_]] {
  def map[A,B](x: F[A], f: A => B): F[B]
}

object OptionFunctor extends Functor[Option] {
  override def map[A, B](x: Option[A], f: A => B): Option[B] =
    x match {
      case None => None
      case Some(value) =>
        Some(f(value))
    }
}

given Functor[Option] with {
  override def map[A, B](x: Option[A], f: A => B): Option[B] =
    x match {
      case None => None
      case Some(value) =>
        Some(f(value))
    }
}

def map[F[_], A, B](x: F[A], f: A => B)(using functor: Functor[F]): F[B] =
  functor.map(x, f)


def foo() = {
  val oi1: Option[Int] = Some(5)
  map(oi1, { (x:Int) => x + 1 })
}

