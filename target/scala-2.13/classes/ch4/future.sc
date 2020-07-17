import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


@volatile var totalA = 0
val text = Future {
  "na" * 16 + "BATMAN!!!!"
}

text foreach (txt =>
  totalA += txt.count(_ == 'a')
  )

text foreach (txt =>
  totalA += txt.count(_ == 'A')
  )


Await.result(text,1.seconds)

totalA
1
