library(plotly)
py <- ggplotly(username="d.kuemper", key="plotLyExperience144")  # open plotly connection

p <- ggplot(data = result.df, aes(x = speed_delta, y = speed_min1)) +
  geom_point(aes(text = paste("Confidence:", confidence)), size = 4) +
  geom_smooth(aes(colour = time_delta, fill = time_delta)) + facet_wrap(~ time_delta)
p
ggplotly(p)
ggplotly()

