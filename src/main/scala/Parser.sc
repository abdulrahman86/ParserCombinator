import Parser._

//string parser
run("hi")("hi i am here")

//slice
run(slice("hi") label("hi expected") scope("Parse Error"))("hit")
