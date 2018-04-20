setClass("TemperatureDegree", contains = c("numeric"))
setClass("Celcius", contains = "TemperatureDegree")
setClass("Faranheit", contains = "TemperatureDegree")
setClass("Kelvin", contains = "TemperatureDegree")

setMethod("plot", "TemperatureDegree",
          function(x, y, ...) {
            plot(density(x), ...)
          })

setMethod("plot", "Celcius", function(x, y, ...) {
   callNextMethod(x, ...)
   abline(v = c(0, 100))
})


cc = new("Celcius", runif(200, -10, 130))

plot(cc)
