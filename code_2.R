library(tidyverse)
library(plotly)

subplot(plot_ly(mpg, x = ~cty, y = ~hwy, name = 'default'), 
        plot_ly(mpg, x = ~cty, y = ~hwy) %>% 
          add_markers(alpha = 0.2, name = 'alpha'), 
        plot_ly(mpg, x = ~cty, y = ~hwy) %>% 
          add_markers(symbol = I(1), name = 'hollow'))


subplot(plot_ly( x= 1:25, y = 1:25,symbol = I(1:25), name = 'pch'), plot_ly(mpg, x = ~cty, y = ~hwy, symbol = ~cyl, symboles = 1:3, name ='cyl'))

p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3) 
subplot(add_markers(p, symbol = ~cyl, name = 'a single trace'), 
        add_markers(p, symbol = ~factor(cyl),color = I('black')))

p <- plot_ly(mpg, x= ~cty, y = ~hwy, alpha = 0.5)
subplot(add_markers(p, color = ~cyl , showlegend = FALSE) %>% 
          colorbar(title = 'Viridis'), 
        add_markers(p, color = ~factor(cyl)))

col1 <- c('#132B43', '#56B1F7')
col2 <- viridisLite::inferno(10)
col3 <-colorRamp(c('red', 'white', 'blue'))

subplot(add_markers(p, color= ~cyl, colors = col1) %>% 
      colorbar(title = "ggplot2 default"),
      add_markers(p, color = ~cyl, colors = col2) %>% 
        colorbar(title = 'Inferno'), 
      add_markers(p, color = ~cyl, colors  = col3) %>% 
        colorbar(title = 'colorRamp') %>% 
        hide_legend())


col1 <- 'Pastel1'
col2 <- colorRamp(c('red', 'blue'))
col3 <- c('4' = 'red', '5' = 'black', '6' = 'blue', '8' = 'green')
subplot(add_markers(p, color = ~factor(cyl), colors = col1),
        add_markers(p, color = ~factor(cyl), colors = col2), 
        add_markers(p, color = ~factor(cyl), colors = col3)) %>% hide_legend()


subplot(add_markers(p, size = ~cyl, name = 'default'), 
        add_markers(p, size= ~cyl, sizes = c(1, 500), name = 'custom'))


plot_ly(mpg, x = ~cty, y = ~hwy, z= ~cyl) %>% add_markers(color = ~cyl)


pm <- GGally::ggpairs(iris)
ggplotly(pm)

m <- lm(Sepal.Length ~Sepal.Width * Petal.Length * Petal.Width, data= iris)

d <- broom::tidy(m) %>% arrange(desc(estimate)) %>% mutate(term = factor(term, levels = term)) 
plot_ly(d, x = ~estimate, y = ~term) %>% add_markers(error_x = ~list(value = std.error)) %>% layout(margin = list(l = 200))

plot_ly(txhousing, x = ~date, y = ~median) %>% add_lines(color  = ~city, colors ='black', alpha = 0.2)

library(dplyr)
top5 <- txhousing %>% group_by(city) %>% summarize(m = mean(sales, na.rm = TRUE)) %>% arrange(desc(m)) %>% top_n(5)

p <- semi_join(txhousing, top5, by = 'city') %>% plot_ly(x= ~date, y = ~median)

subplot(add_lines(p, color= ~city), add_lines(p, linetype = ~city), shareX = TRUE, nrows = 2 )

kerns = c('gaussian', 'epanechnikov', 'rectangular', 'triangular', 'biweight', 'cosine', 'optcosine')
p <- plot_ly()
for (k in kerns){
  d <- density(txhousing$median, kernel = k, na.rm = TRUE)
  p <- add_lines(p, x = d$x,y = d$y, name= k)
}
layout(p, xaxis = list(title = 'Median monthly price'))

iris$obs <- seq_len(nrow(iris))
iris_pcp <- function(transform = identity){
  iris[] <- purrr::map_if(iris, is.numeric, transform)
  tidyr::gather(iris, variable, value, -Species, -obs) %>% group_by(obs) %>% plot_ly(x = ~variable, y = ~value, color = ~Species) %>% add_lines(alpha = 0.3)
}
subplot(iris_pcp(), iris_pcp(scale), iris_pcp(scales::rescale)) %>% hide_legend()


plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>% add_paths(color = ~displ)

plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>% add_lines(color = ~displ)

library(quantmod)
msft <- getSymbols('MSFT', auto.assign = F)

dat <- as.data.frame(msft)
dat$date<- index(msft)
dat <- subset(dat, date >= '2016-01-01')
names(dat) <- sub('^MSFT\\.', '', names(dat))

plot_ly(dat, x= ~date, xend= ~date, color = ~Close >Open,  colors = c('red', 'forestgreen'), hoverinfo ='none') %>% add_segments(y = ~Low,  yend = 'High', size = I(1)) %>% add_segments(y = ~Open, yend = ~Close, size = I(3)) %>% 
  layout(showlegend = FALSE, yaxis = list(title = 'Price')) %>% rangeslider()


plot_ly(dat, x= ~date, xend= ~date, color = ~Close >Open,  colors = c('red', 'forestgreen'), hoverinfo ='none')  %>% 
  layout(showlegend = FALSE, yaxis = list(title = 'Price')) %>% rangeslider()


m <- lm(mpg ~wt, data = mtcars)
broom::augment(m) %>% plot_ly(x = ~wt,showlegend = FALSE) %>% add_markers(y = ~mpg, color = I('black')) %>% add_ribbons(ymin = ~.fitted - 1.96 *.se.fit, ymax = ~.fitted + 1.96 * .se.fit, color = I('gray80')) %>% add_lines(y = ~.fitted, color = I('steelblue'))

map_data('world', 'canada') %>% group_by(group) %>% plot_ly(x = ~long, y = ~lat, alpha = 0.2) %>% add_polygons(hoverinfo = 'none', color = I('black')) %>% add_markers(text = ~paste(name, '<br />', pop), hoverinfo = 'text', color = I('red'), data= maps::canada.cities) %>% layout(showlegend = FALSE)

dat <- map_data('world', 'canada') %>% group_by(group)

library(tidyverse)
library(plotly)

map1 <- plot_mapbox(dat, x= ~long, y  = ~lat) %>% add_paths(size = I(2)) %>% add_segments(x = -100, xend = -50, y = 50, 75) %>% layout(mapbox= list(zoom = 0, center = list(lat = ~median(lat), lon = ~median(long))))

map2 <- plot_geo(dat, x  = ~long, y = ~lat) %>% add_markers(size = I(1)) %>% add_segments(x = -100, xend = -50, y = 50, 75) %>% layout(geo = list(projection = list(type = 'mercator')))

map3 <- plot_ly(dat, x = ~long, y = ~lat) %>% add_paths(size = I(1)) %>% add_segments(x = -100, xend = -50, y = 50, 75) %>% layout(xaxis = list(scaleanchor = 'y', scaleratio = 1))

htmltools::tagList(map1, map2, map3)

density <-state.x77[, 'Population']/state.x77[, 'Area']

g <- list(scope = 'usa', 
          projection = list(type = 'albers usa'), 
          lakecolor = toRGB('white'))

plot_geo() %>% add_trace(z = ~density, text = state.name, locations = state.abb, locationmode = "USA-states") %>% add_markers(x = state.center[['x']], y = state.center[['y']], size = I(2), symbol = I(8), color = I('white'), hoverinfo = 'none') %>% layout(geo = g)


nc <- sf::st_read(system.file('shape/nc.shp', package = 'sf'), quiet = TRUE)

p <- ggplot(nc) + geom_sf(aes(fill = AREA))
ggplotly(p)

basemap <- get_map(maptype = 'satellite', zoom = 8)

ggplotly(ggmap(basemap))

# bars and histograms
p1 <- plot_ly(diamonds, x = ~price) %>% add_histogram(name = 'plotly.js')

price_hist <- function(method = 'FD'){
  h <- hist(diamonds$price, breaks = method, plot = FALSE)
  plot_ly(x = h$mids, y = h$counts) %>% add_bars(name = method)
}

subplot(p1, price_hist(), price_hist('Sturges'), price_hist('Scott'), nrows= 4, shareX = TRUE)

p1 <- plot_ly(diamonds, x = ~cut) %>% add_histogram()

p2 <- diamonds %>% count(cut) %>% plot_ly(x= ~cut, y = ~n) %>% add_bars()

subplot(p1, p2) %>% hide_legend()

one_plot <- function(d){
  plot_ly(d, x = ~price) %>% add_annotations(~unique(clarity), x = 0.5, y = 1, xref = 'paper', yref= 'paper', showarrow = FALSE)
}

diamonds %>% split(.$clarity) %>% lapply(one_plot) %>% subplot(nrows = 2, shareX = TRUE, titleX = FALSE) %>% hide_legend()

plot_ly(diamonds, x = ~cut, color = ~clarity) %>% add_histogram()

cc <- count(diamonds, cut, clarity)

cc2 <- left_join(cc, count(cc, cut, wt = n))

cc2 %>% mutate(prop = n/nn) %>% plot_ly(x= ~cut, y = ~prop, color = ~clarity) %>% add_bars() %>% layout(barmode = 'stack')

library(ggmosaic)
p <- ggplot(data = cc) + geom_mosaic(aes(weight  = n, x= product(cut), fill = clarity))
ggplotly(p)

p <- plot_ly(diamonds, y = ~price, color = I('black'), alpha = 0.1, boxpoints = 'suspectedoutliers')
p1 <- p %>% add_boxplot(x = 'Overall')
p2 <- p %>% add_boxplot( x= 'cut')
subplot(p1, p2, shareY = TRUE, widths = c(0.2, 0.8), margin = 0) %>% hide_legend(
)

plot_ly(diamonds, x = ~price, y = ~interaction(clarity, cut)) %>% add_boxplot(color = ~clarity) %>% layout(yaxis = list(title = ''), margin = list(l = 100
                                             ))

d <- diamonds %>% mutate(cc = interaction(clarity, cut))

lvls <- d %>% group_by(cc) %>% summarize(m = median(price)) %>% arrange(m) %>% .[['cc']]

plot_ly(d, x = ~price, y = ~factor(cc, lvls)) %>% add_boxplot(color = ~clarity) %>% layout(yaxis = list(title = ''), margin = list(l = 100))

# 2.5 2D frequencies
p <- plot_ly(diamonds, x = ~log(carat), y = ~log(price))

subplot(add_histogram2d(p) %>% colorbar(title = 'default') %>% 
          layout(xaxis = list(title = 'default')), 
        add_histogram2d(p, zsmooth = 'best') %>% 
          layout(xaxis = list(title = 'zsmooth')
                   ), add_histogram2d(p, nbinsx = 60, nbinsy = 60) %>% colorbar(title = 'nbins') %>% layout(xaxis  = list(title = 'nbins')), shareY = TRUE, titleX = TRUE)

kde_count <- function(x, y , ...){
  kde <- MASS::kde2d(x, y, ...)
  df <- with(kde, setNames(expand.grid(x, y), c('x', 'y')))
  df$count <- with(kde, c(z) *length(x) * diff(x)[1]* diff(y)[1])
  data.frame(df)
}
kd <- with(diamonds, kde_count(log(carat), log(price), n = 30))

plot_ly(kd, x = ~x, y = ~y, z = ~count) %>% add_heatmap() %>% colorbar(title = 'Number of diamonds')

corr <- cor(diamonds[vapply(diamonds, is.numeric, logical(1))])

plot_ly(x  = rownames(corr), y = colnames(corr), z = corr) %>% add_heatmap() %>% colorbar(limits= c(-1, 1))

x <- seq_len(nrow(volcano)) + 100
y <- seq_len(ncol(volcano)) + 500

plot_ly() %>% add_surface(x = ~x, y = ~y, z = ~volcano)


# 2.7 raster images
m <- matrix(hcl(0, 80, seq(50,80, 100)), nrow = 4, ncol = 5)

r <-as.raster(m)
plot(r)

plot_ly() %>% layout(images = list(source = raster2uri(r), xref = 'x', yref= 'y', x = 0, y = 0, sizex = 1, sizey= 1, sizing = 'stretch', xanchor = 'left', yanchor = 'bottom'))

vars <- setdiff(names(economics), 'date')

plots <- lapply(vars, function(var){
  plot_ly(economics, x = ~date, y = as.formula(paste0('~', var))) %>% add_lines(name = var)
})

subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

economics %>% tidyr::gather(variable, value, -date) %>% transform(id = as.integer(factor(variable))) %>% plot_ly(x = ~date, y = ~value, color = ~variable, colors = 'Dark2', yaxis = ~paste0('y', id)) %>% add_lines() %>% subplot(nrows = 5, shareX = TRUE)

x <- rnorm(100)
y <- rnorm(100)

s <- subplot(plot_ly(x = x, color = I('black')), plotly_empty(), plot_ly(x=x, y = y, color = I('black')), plot_ly(y = y, color = I('black')), nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
             )

layout(s, showlegend = FALSE)


plotList <- function(nplots){
  lapply(seq_len(nplots), function(x) plot_ly())
}
s1 <- subplot(plotList(6), nrows = 2, shareX = TRUE, shareY = TRUE)
s2 <- subplot(plotList(2), shareY = TRUE)
subplot(s1, s2, plot_ly(), nrows = 3, margin = 0.04, heights = c(0.6, 0.3, 0.1))

g <- list(scope = 'usa', projection = list(type = 'albers usa'), lakecolor = toRGB('white'))

ddensity <- state.x77[, 'Population']/state.x77[, 'Area']

map <- plot_geo(z = ~density, text = state.name, locations = state.abb, locationmode = 'USA-states') %>% layout(geo = g)
vars <- colnames(state.x77)
barcharts <- lapply(vars, function(var){
  plot_ly(orientation = 'h', name = var) %>% layout(orientation = 'h', name = var) %>% layout(showlegend = FALSE, hovermode = 'y', yaxis = list(showticklabels = FALSE))
})

subplot(subplot(barcharts, margin = 0.01), map, nrows= 2, heights = c(0.3, 0.7), margin = 0.1)

e <- gather(economics, variable, value, -date)
gg1 <- ggplot(e, aes(date, value))+geom_line() + facet_wrap(~variable, scales = 'free_y', ncol = 1)
gg2 <- ggplot(e, aes(factor(1), value)) + geom_violin() + facet_wrap(~variable, scales = 'free_y', ncol = 1) + theme(axis.text = element_blank(), axis.ticks = element_blank())
subplot(gg1, gg2) %>% layout(margin = list(l = 50))


# 3.3 navigating many views

# linking views without shiny
library(crosswalk)
sd <- SharedData$new(txhousing, ~year)

p <- ggplot(sd, aes(month, median)) + geom_line(aes(group = year))  + geom_smooth(data = txhousing, method = 'gam') + facet_wrap(~city)
ggplotly(p, tooltip = 'year') %>% highlight(defaultValues = 2015, color = 'red')

d <- SharedData$new(iris)
p <- GGally::ggpairs(d, aes(color = Species), columns = 1:4)
highlight(ggplotly(p), on = 'plotly_selected')


sd <- SharedData$new(txhousing, ~city)
p <- ggplot(sd, aes(date, median))+ geom_line() 
gg <- ggplotly(p, tooltuip = 'city')
highlight(gg, on = 'plotly_hover', dynamic = TRUE)
highlight(gg, on = 'plotly_hover', dynamic = TRUE, persistent = TRUE)


library(plotly)
require(leaflet)
sd <- SharedData$new(quakes)
options(persistent = TRUE)
p <- plot_ly(sd, x =~depth, y = ~mag) %>% add_markers(alpha = 0.5) %>% highlight('plotly_selected', dynamic = TRUE)
map <- leaflet(sd) %>% addTiles() %>% addCircles()

bscols(widths = c(6, 6), p, map)

sd <- SharedData$new(txhousing, ~city, group = 'choose a city')
plot_ly(sd, x = ~date, y = ~median) %>% group_by(city) %>% add_lines(text = ~city, hoverinfo = 'text') %>% highlight(on = 'plotly_hover', persistent = TRUE, selectize = TRUE)

sd <- highlight_key(txhousing, ~city, 'Select a city')
base <-plot_ly(sd, color = I('black'), height = 400) %>% group_by(city)

p1 <- base %>% summarize(miss = sum(is.na(median))) %>% filter(miss >0) %>% add_markers(x = ~miss, y =~forcats::fct_reorder(city, miss), hoverinfo = 'x + y') %>% layout(barmode = 'overlay', xaxis = list(title = 'Number of months missing'), yaxis  = list(title = ''))

subplot(p1, p2, titleX = TRUE, widths = c(0.3, 0.7)) %>% hide_legend() %>% highlight(dynamic= TRUE, selectize = TRUE)

d <- SharedData$new(mtcars)
scatterplot <- plot_ly(d, x = ~mpg, y = ~disp) %>% add_markers(color = I('black'))
subplot(plot_ly(d, y = ~disp, color = I('black')) %>% add_boxplot(name = 'overall'), scatterplot, shareY  = TRUE) %>% highlight('plotly_selected')

p <- subplot( plot_ly(d, x = ~factor(vs)) %>% add_histogram(color = I('black')), scatterplot)

p %>% layout(barmode = 'overlay') %>% highlight('plotly_selected')

m <- SharedData$new(mpg)
p <- ggplot(m , aes(displ, hwy, color = class)) + geom_point() + geom_smooth(se = FALSE, method= 'lm')
ggplotly(p) %>% highlight('plotly_hover')

mtcars$am <- recode(mtcars$am, '0' = 'automatic', '1' = 'manual')
mod <- step(lm(mpg~., data = mtcars), trace  = FALSE)
pm <- GGally::ggnostic(mod, mapping = aes(color = am))
ggplotly(pm) %>% highlight('plotly_click')

m <- SharedData$new(mpg)
p1 <- ggplot(m, aes(displ, fill = class)) + geom_density()
p2 <- ggplot(m , aes(displ, hwy, fill = class)) + geom_point()
subplot(p1, p2) %>% highlight('plotly_click') %>% hide_legend()


d <- data.frame(x = 1:4, y = 1:4)
d$key <- lapply(1:4, function(x) letters[seq_len(x)])
d

SharedData$new(d, ~key) %>% plot_ly(x = ~x, y = ~y) %>% highlight('plotly_selected') %>% layout(dragmode = 'lasso')


# 5 animating views ----

