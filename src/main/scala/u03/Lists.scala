package u03

import scala.annotation.tailrec

enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n - 1)
      case Cons(h, t) => Cons(h, t)
      case Nil() => Nil()

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case _ => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case _ => Nil()

    def mapWithFlatMap[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(v => Cons(mapper(v), Nil()))

    def filterWithFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] =
      flatMap(l1)(f => f match
        case f if pred(f) => Cons(f, Nil())
        case _ => Nil()
      )

    @tailrec
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, Cons(h2, t2)) if h >= h2 => max(Cons(h, t2))
      case Cons(h, Cons(h2, t2)) if h < h2 => max(Cons(h2, t2))
      case Cons(h, Nil()) => Some(h)
      case _ => None

    def listOfCourses(l: List[Person]): List[String] =
      flatMap(l)(f => f match
        case Person.Teacher(name, course) => Cons(course, Nil())
        case _ => Nil()
      )

    @tailrec
    def foldLeft[A, B](l: List[A])(initialValue: B)(f: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(initialValue, h))(f)
      case _ => initialValue

    def foldRight[A, B](l: List[A])(initialValue: B)(f: (A, B) => B): B = l match
      case Cons(h, Nil()) => f(h, initialValue)
      case Cons(h, t) => f(h, foldRight(t)(initialValue)(f))
      case _ => initialValue


  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52


