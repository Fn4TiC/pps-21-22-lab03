package u03

import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Lists.List.*
import u03.Streams.Stream.*

class StreamTest:

  import Streams.*

  private var stream: Stream[Int] = take(iterate(0)(_ + 1))(10)

  @Test def testDrop(): Unit =
    assertEquals(Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil()))))))), toList(Stream.drop(stream)(3)))
    assertEquals(Nil(), toList(Stream.drop(stream)(10)))

  @Test def testConstant(): Unit =
    assertEquals(Cons("ggg", Cons("ggg", Cons("ggg", Nil()))), toList(take(constant("ggg"))(3)))

  @Test def testInfiniteFib(): Unit =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Cons(21, Cons(34, Cons(55, Cons(89, Cons(144, Nil()))))))))))))), toList(Stream.take(Stream.fibs)(13)))
