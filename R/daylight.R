
# adapted from http://r.789695.n4.nabble.com/maptools-sunrise-sunset-function-td874148.html
ephemeris <- function(lat, lon, date, span=1, tz="UTC") {

  lon.lat <- matrix(c(lon, lat), nrow=1)

  # using noon gets us around daylight saving time issues
  day <- as.POSIXct(sprintf("%s 12:00:00", date), tz=tz)
  sequence <- seq(from=day, length.out=span , by="days")

  sunrise <- sunriset(lon.lat, sequence, direction="sunrise", POSIXct.out=TRUE)
  sunset <- sunriset(lon.lat, sequence, direction="sunset", POSIXct.out=TRUE)

  # dawn <- sunriset(lon.lat, sequence, direction="dawn", POSIXct.out=TRUE)
  # dusk <- sunriset(lon.lat, sequence, direction="dusk", POSIXct.out=TRUE)

  solar_noon <- solarnoon(lon.lat, sequence, POSIXct.out=TRUE)

  data.frame(date=as.Date(sunrise$time),
             sunrise=as.numeric(format(sunrise$time, "%H%M")),
             # dawn=as.numeric(format(dawn$time, "%H%M")),
             solarnoon=as.numeric(format(solar_noon$time, "%H%M")),
             # dusk=as.numeric(format(dusk$time, "%H%M")),
             sunset=as.numeric(format(sunset$time, "%H%M")),
             day_length=as.numeric(sunset$time-sunrise$time))

}

# for graph #1 y-axis
time_format <- function(hrmn) substr(sprintf("%04d", hrmn),1,2)

# for graph #2 y-axis
pad5 <- function(num) sprintf("%2d", num)


daylight <- function(lat, lon, place, start_date, span=2, tz="UTC", show_solar_noon=FALSE, show_now=TRUE, plot=TRUE) {

  stopifnot(span>=2)

  srss <- ephemeris(lat, lon, start_date, span, tz)

  View(srss)

  today = as.Date(Sys.time())
  yesterday = as.Date(Sys.time()) - 1


  sunrise_today <- srss %>% filter(date == today) %>% select(sunrise) %>%  pull()
  noon_today <- srss %>% filter(date == today) %>% select(solarnoon) %>%  pull()
  sunset_yesterday <- srss %>% filter(date == yesterday) %>% select(sunset) %>%  pull()


  daylight_minutes_today <- srss %>% filter(date == today) %>% select(day_length) %>%  pull()
  daylight_minutes_yesterday <- srss %>% filter(date == yesterday) %>% select(day_length) %>%  pull()

  today_additional_minutes <- round((daylight_minutes_today - daylight_minutes_yesterday) * 100, 0)

  # Get prev and next Sunday to diff daylight hours
  d<-today
  sunday_prev <- d-as.POSIXlt(d)$wday
  sunday_next <- sunday_prev + 7

  daylight_minutes_sunday_prev <- srss %>% filter(date == sunday_prev) %>% select(day_length) %>%  pull()
  daylight_minutes_sunday_next <- srss %>% filter(date == sunday_next) %>% select(day_length) %>%  pull()

  daylight_minutes_longer_this_week <- daylight_minutes_sunday_next - daylight_minutes_sunday_prev

  print(today)
  print(yesterday)

  print(daylight_minutes_today)
  print(daylight_minutes_yesterday)
  print(today_additional_minutes)


  print(paste("sunday_prev: ", sunday_prev, sep=""))
  print(paste("sunday_next: ", sunday_next, sep=""))

  print(paste("daylight_minutes_sunday_prev: ", daylight_minutes_sunday_prev, sep=""))
  print(paste("daylight_minutes_sunday_next: ", daylight_minutes_sunday_next, sep=""))
  print(paste("daylight_minutes_longer_this_week: ", daylight_minutes_longer_this_week, sep=""))

  minutes_this_week <- hms::hms(minutes = (daylight_minutes_longer_this_week * 100))

  print(paste("minutes_this_week:: ", minutes_this_week, sep=""))

  minutes_this_week_time60 <- minutes_this_week / 60

  print(paste("minutes_this_week_time60: ", minutes_this_week_time60, sep=""))

  this_week_minutes <- dminutes(minutes_this_week_time60)

  print(paste("this_week_minutes: ", this_week_minutes, sep=""))

  x_label = ""





  gg <- ggplot(srss, aes(x=date))
  gg <- gg + geom_ribbon(aes(ymin=sunrise, ymax=sunset), fill="#FFD301")

  # Annotate today
  if (show_now) {
    gg <- gg + geom_vline(xintercept=as.numeric(as.Date(Sys.time())), color="#ffffff", linetype="solid", size=0.5)
    gg <- gg + geom_vline(xintercept=as.numeric(as.Date(Sys.time())), color="#111111", linetype="dotted", size=0.5)
  }


  gg <- gg + scale_x_date(expand=c(0,0),     labels = scales::label_date_short())
  gg <- gg + scale_y_continuous(labels=time_format, limits=c(0,2400), breaks=seq(0, 2400, 200), expand=c(0,0))
  gg <- gg + labs(x=x_label, y="",
                  title=sprintf("Sunrise & sunset for %s %s ", place, paste0("", collapse="")
                                ) )
  gg <- gg +   ggthemes::theme_few()
  gg <- gg +   theme_waapihk()
  gg <- gg + theme(panel.background=element_rect(fill="#02198B"))
  gg <- gg + theme(plot.title = ggplot2::element_text(size=20, lineheight=1.2, face="bold", color="#222222", margin=ggplot2::margin(0,0,5,0)))
  gg <- gg + theme(panel.border = ggplot2::element_blank())


  gg1 <- ggplot(srss, aes(x=date, y=day_length))
  gg1 <- gg1 + geom_area(fill="#FFD301", alpha=1) #fffd37
  # gg1 <- gg1 + geom_line(color="#B0DBF1", size=1) #fffd37

  # Annotate today
  if (show_now) {
    gg1 <- gg1 + geom_vline(xintercept=as.numeric(as.Date(Sys.time())), color="#ffffff", linetype="solid", size=0.5)
    gg1 <- gg1 + geom_vline(xintercept=as.numeric(as.Date(Sys.time())), color="#111111", linetype="dotted", size=0.5)
  }

  gg1 <- gg1 + annotate("text", x = today + 5, y = daylight_minutes_today, label = paste(format(today, "%B %d"), sep=""), hjust=0, size=5, fontface = "bold", vjust = 1.5)
  gg1 <- gg1 + annotate("text", x = today + 5, y = daylight_minutes_today, label = paste("~ ", today_additional_minutes, " minutes of daylight \nmore than yesterday", sep=""), hjust=0, vjust = 2, size=4, lineheight=1)

  gg1 <- gg1 + scale_x_date(expand=c(0,0),     labels = scales::label_date_short())
  gg1 <- gg1 + scale_y_continuous(labels=pad5, limits=c(0,24), breaks=seq(0, 24, 5), expand=c(0,0))

  gg1 <- gg1 + labs(x=x_label, y="",
                  title=sprintf("Daylight hours for %s %s ", place, paste0("", collapse="")
                  ) )
  gg1 <- gg1 +   ggthemes::theme_few()
  gg1 <- gg1 +   theme_waapihk()
  gg1 <- gg1 + theme(panel.background=element_rect(fill="#02198B"))
  gg1 <- gg1 + theme(plot.title = ggplot2::element_text(size=20, lineheight=1.2, face="bold", color="#222222", margin=ggplot2::margin(0,0,5,0)))
  gg1 <- gg1 + theme(panel.border = ggplot2::element_blank())

  if (plot) grid.arrange(gg, gg1, nrow=2)

  arrangeGrob(gg, gg1, nrow=2)

  # plot(gg)

  plot(gg1)

  # finalize_plot(gg1)

  invisible(srss)

}

sunrise_sunset <- grid::grid.draw(daylight(49.8954, -97.1385, "Winnipeg, Manitoba", "2023-01-01", 365, tz="America/Winnipeg"))
