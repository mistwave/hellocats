import cats.data.Reader

case class Cat(name: String, favoriteFood: String)


val catName: Reader[Cat, String] =
  Reader(cat => cat.name)

val cc = Cat("Nat", "dry fish")

val ff = Cat("F", "junk food")

catName.run(cc)

val greetKitty: Reader[Cat, String] =
  catName.map(name => s"Hello ${name}")


greetKitty.run(cc)


val feedKitty : Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")


val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetKitty
    feed <- feedKitty
  } yield s"$greet. $feed."


greetAndFeed(cc)