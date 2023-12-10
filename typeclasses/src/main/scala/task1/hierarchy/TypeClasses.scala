package task1.hierarchy
import task1._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Apply[F[_]] extends Functor[F] {
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
}

trait FlatMap[F[_]] extends Apply[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object TypeClasses {
  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value)         => Leaf(f(value))
    }
  }

  implicit object TreeApply extends Apply[Tree] {
    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = (ff, fa) match {
      case (Branch(fLeft, fRight), Branch(xLeft, xRight)) =>
        Branch(ap(fLeft)(xLeft), ap(fRight)(xRight))
      case (Leaf(f), Leaf(x)) => Leaf(f(x))
      case _                  => throw new IllegalArgumentException("Incompatible shapes for ap")
    }
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = TreeFunctor.map(fa)(f)
  }

  implicit object TreeApplicative extends Applicative[Tree] {
    def pure[A](a: A): Tree[A] = Leaf(a)
    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = TreeApply.ap(ff)(fa)
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = TreeApply.map(fa)(f)
  }

  implicit object TreeFlatMap extends FlatMap[Tree] {
    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value)         => f(value)
    }

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = TreeApply.ap(ff)(fa)
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = TreeApply.map(fa)(f)
  }

  implicit object TreeMonad extends Monad[Tree] {
    def pure[A](a: A): Tree[A] = TreeApplicative.pure(a)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = TreeFlatMap.flatMap(fa)(f)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] = TreeApply.ap(ff)(fa)
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = TreeApply.map(fa)(f)
  }
}
