library(tidyverse)
plot_ly(z = ~volcano)

library(plotly)
txhousing
p <- ggplot(txhousing, aes(date, median)) + geom_line(aes(group = city), alpha = 0.2)
p

subplot(p, ggplotly(p, tooltip = 'city'), ggplot(txhousing, aes(date, median)) + geom_bin2d(), ggplot(txhousing, aes(date, median)) + geom_hex(), nrows = 2, shareX = TRUE, shareY = TRUE, titleY = FALSE, titleX = FALSE)

library(dplyr)
tx <- group_by(txhousing, city)
p <- plot_ly(tx, x = ~date, y = ~median)
plotly_data(p)

add_lines(add_lines(p, alpha = 0.2, name = 'Texan Cities', hoverinfo = 'none'), name = 'Houston', data = filter(txhousing, city == 'Houston'))

allCities <- txhousing %>%  group_by(city) %>% plot_ly(x = ~date, y = ~median) %>% add_lines(alpha = 0.2, name = 'Texan Cities', hoverinfo = 'none')

allCities %>% filter(city == 'Houston') %>% add_lines(name = 'HOuston')

allCities %>% add_fun(function(plot) {
  plot %>% filter(city == "Houston") %>% add_lines(name = 'Houston')}) %>% add_fun(function(plot){
    plot %>% filter(city == 'San Antonio') %>% add_lines(name = 'San Antonio')
})


layer_city <- function(plot, name){
  plot %>% filter(city == name) %>% add_lines(name = name)
}

layer_iqr <- function(plot){
  plot %>% group_by(date) %>% summarize(q1 = quantile(median, 0.25, na.rm = TRUE), m = median(median, na.rm = TRUE), q3 = quantile(median, 0.75, na.rm = TRUE)) %>% add_lines(y  = ~m, name = 'median', color = I('black')) %>% add_ribbons(ymin = ~q1, ymax = ~q3, name = 'IQR', color = I('black'))
}

allCities %>% add_fun(layer_iqr) %>% add_fun(layer_city, 'Houston') %>% add_fun(layer_city, 'San Antonio')

library(forecast)
layer_forecast <- function(plot){
  d <- plotly_data(plot)
  series <- with(d, ts(median, frequency = 12, start= c(2000,1 ), end = c(2015, 7)))
  fore <- forecast(ets(series))
  plot %>% add_ribbons(x = time(fore$mean), 
                       ymin = fore$lower[, 2], 
                       ymax = fore$upper[, 2], color = I('gray95'),
                       name = '95% confidence', inherit = FALSE) %>% 
    add_ribbons(x = time(fore$mean), ymin = fore$lower[, 1], ymax = fore$upper[, 1], color = I('gray80'), 
                name = '80% confidence', inherit = FALSE) %>% add_lines(x = time(fore$mean), y = fore$mean, color = I("blue"), name ='prediction')
  }

txhousing %>% group_by(city) %>% plot_ly(x = ~date, y = ~median) %>% add_lines(alpha = 0.2, name= 'Texan Cities', hoverinfo = 'none') %>% add_fun(layer_iqr) %>% add_fun(layer_forecast)

p <- ggplot(fortify(gold), aes(x, y )) + geom_line()
gg <- ggplotly(p)
layout(gg, dragmode = 'pan')

rangeslider(gg)




