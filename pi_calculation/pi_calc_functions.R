# график
plot_calc_pi <- function(n) {
    x <- runif(n, min = -1, max = 1)
    y <- runif(n, min = -1, max = 1)
    dist <- x^2 + y^2
    a <- ifelse(dist < 1, 1, 0)
    data <- data.frame(x = x, y = y, dist = dist, a = a)
    
    circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
        r = diameter / 2
        tt <- seq(0,2*pi,length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
    }
    circle <- circleFun(c(0, 0), 2, npoints = 100)
    
    ggplot(data, aes(x = x, y = y)) +
        geom_point(aes(color = as.factor(a))) +
        geom_rect(aes(xmin = -1, ymin = -1, xmax = 1, ymax = 1), 
                  color = "black",
                  fill = NA) +
        geom_path(aes(x, y), circle) +
        ggtitle(paste("pi =", 4*sum(a)/length(a))) +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5))
}

#просто подсчет
calc_pi <- function(n) {
    x <- runif(n, min = -1, max = 1)
    y <- runif(n, min = -1, max = 1)
    dist <- x^2 + y^2
    a <- ifelse(dist < 1, 1, 0)
    4*sum(a)/length(a)
}

# распределение
x <- numeric(1000)
for(i in 1:length(x)) {
    x[i] <- calc_pi(10000)
}
hist(x, 
     xlab = paste0("Значения числа Пи (среднее значение = ", mean(x), ")"), 
     main = "Распределение значения Пи")
abline(v = mean(x), col = "red")
