# need to fix hover - done!
# need to fix issue of variable existing, but wanting data first - done!
# no clue how to subset a ts object properly - done! (do not pass on bad attrs)

# missing functions or extra arguments - not fixed


ir <- iris
ir$glyphVal <- as.numeric(ir$Species)
ir$glyphCol <- c("red", "green", "blue")[ ir$glyphVal ]
ir$randomGroup <- sample(c("A", "B"), nrow(ir), replace = TRUE)

rescale <- function(x) {
  (x - min(x)) / diff(range(x))
}

bFig <- figure(width = 480*1.5,height = 520*1.5)


# works!
load_all(); bFig %>% ly_points(Sepal.Length, Sepal.Width, data = ir, hover = Species)
load_all(); bFig %>% ly_points(Sepal.Length, Sepal.Width, data = ir, hover = list(Species, pl = Petal.Length, Petal.Length))
load_all(); bFig %>% ly_points(Sepal.Length, Sepal.Width, data = ir, hover = "Species")
load_all(); specs <- c("Species", "Sepal.Width"); bFig %>% ly_points(Sepal.Length, Sepal.Width, data = ir, hover = specs)
load_all(); bFig %>% ly_points(Sepal.Length, Sepal.Width, data = ir, hover = NULL)
load_all(); bFig %>% ly_points(Sepal.Length, Sepal.Width, data = ir, hover = "Species")

# why?
load_all(); bFig %>% ly_points(Sepal.Length, Sepal.Width, data = ir, hover = "@Species")


library(maps)
data(world.cities)
caps <- subset(world.cities, capital == 1)
caps$population <- prettyNum(caps$pop, big.mark = ",")

a <- figure(width = 800, padding_factor = 0) %>%
  ly_map("world", col = "gray") %>%
  ly_points(long, lat, data = caps, size = 5,
    hover = c(name, country = country.etc, population))
a
# hover issue / fixed


orstationc <- read.csv("http://geog.uoregon.edu/bartlein/old_courses/geog414s05/data/orstationc.csv")
gmap(lat = 44.1, lng = -120.767, zoom = 6, width = 700, height = 600) %>%
  ly_points(lon, lat, data = orstationc, alpha = 0.8, col = "red",
    hover = c(station, Name, elev, tann))
# hover issue / fixed


load_all()
p <- figure(width = 800, height = 400) %>%
  ly_lines(date, Freq, data = flightfreq, alpha = 0.3) %>%
  ly_points(date, Freq, data = flightfreq,
    hover = list(date, Freq, dow), size = 5) %>%
  ly_abline(v = as.Date("2001-09-11"))
p
# namespace issue / fixed
# hover issue / fixed



load_all()
url <- c("http://bokeh.pydata.org/en/latest/_static/bokeh-transparent.png",
  "http://developer.r-project.org/Logo/Rlogo-4.png")
ss <- seq(0, 2*pi, length = 13)[-1]
ws <- runif(12, 2.5, 5) * rep(c(1, 0.8), 6)
imgdat <- data.frame(
  x = sin(ss) * 10, y = cos(ss) * 10,
  w = ws, h = ws * rep(c(1, 0.76), 6),
  url = rep(url, 6)
)
p <- figure(xlab = "x", ylab = "y") %>%
  ly_image_url(x, y, w = w, h = h, imageUrl = url, data = imgdat,
    anchor = "center") %>%
  ly_lines(sin(c(ss, ss[1])) * 10, cos(c(ss, ss[1])) * 10,
    width = 15, alpha = 0.1)
p
# eval issue / fixed





load_all(); figure(xlim = c(-0.1, 1.35), width = 550) %>% ly_quantile(height, group = voice.part, data = lattice::singer)
# legend issue / fixed


# eval issue / fixed
load_all(); figure() %>% ly_points(speed, dist, data = cars)



# eval issue / fixed
load_all(); figure() %>% ly_points(speed, dist^2, data = cars)


# eval issue
# hover issue
load_all(); figure() %>% ly_points(speed, dist, data = cars, hover = c(speed, dist))


# eval issue / fixed
# hover issue / fixed
load_all(); figure() %>% ly_points(speed, dist, data = cars, hover = "Car went @speed mph for @dist dist!")



# ts issue / fixed
co2dat <- data.frame(
  y = co2,
  x = floor(time(co2)),
  m = rep(month.abb, 39))
load_all(); figure() %>% ly_lines(x, y, group = m, data = co2dat)


# ts issue / fixed
load_all(); figure(xlim = c(1958, 2010)) %>% ly_lines(x, y, color = m, data = co2dat)






# arg 'formats' doesn't exist
figure() %>%
  ly_lines(seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by="days"),
           rnorm(366)) %>%
  x_axis(label = "Date", formats = list(months="%b"))


# 'legend_control' function doesn't exist
figure() %>%
  ly_quantile(Sepal.Length, group = Species, data = iris) %>%
  legend_control(orientation = "top_left")
