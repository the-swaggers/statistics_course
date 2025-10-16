# cw 2 9X2025

X=c("K","K","Dz","K","M","Dz","M","K")
X
Y=factor(X)
Y
summary(Y)
Z=factor(X,levels=c("Dz","K","M","NN"))
Z
summary(Z)
table(Y)

https://www.tidyverse.org/
  
  https://dplyr.tidyverse.org/
  https://cran.rstudio.com/web/packages/dplyr/vignettes/dplyr.html

library(dplyr)

library(nycflights13)
data(flights)
dim(flights)
head(flights)
summary(flights)
table(flights$origin)

#filter wybor wierszy

t0=proc.time()[3]
d=filter(flights, month == 10, day == 11,air_time>100)
proc.time()[3]-t0
class(d)

t0=proc.time()[3]
dd=flights[flights$month == 10 & flights$day == 11 & flights$air_time>100, ]
proc.time()[3]-t0

filter(flights,month==1 | day==1)

# slice wybor wierszy wg numeru/pozycji
slice(flights, 10:12)
flights[10:12,]

# arrange sortowanie

arrange(flights, year, month, day)

flights[order(flights$year, flights$month, flights$day), ]

t0=proc.time()[3]
arrange(flights, desc(arr_delay))
proc.time()[3]-t0

t0=proc.time()[3]
flights[order(flights$arr_delay, decreasing = TRUE), ]
proc.time()[3]-t0

# select() wybor kolumn

select(flights, year, month, day)

select(flights, year:day)

select(flights, -(year:day))

select(flights,1:3)

# rename()

rename(flights, tail_num = tailnum)
flights

#distinct() pokazuje unikatowe wiersze

distinct(flights, tailnum)
distinct(flights, origin, dest)
distinct(flights, dest)
distinct(flights, diff = abs(dep_time-arr_time))

# mutate() dodawanie kolumn ktore sa wynikiem operacji na pozostalych

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)


# transmute()

transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)


#summarise


summarise(flights, delay = mean(dep_delay, na.rm = TRUE))


#sample_n()  sample_frac()

sample_n(flights, 10)
sample_frac(flights, 0.001)
sample_n(flights,10,replace = TRUE )


# group_by

by_tailnum <- group_by(flights, tailnum)
class(by_tailnum)
print(by_tailnum)
attr(by_tailnum,"groups")[[1]][1]
attr(by_tailnum,"groups")[[2]][1]

delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay
arrange(delay,desc(count))
delay <- filter(delay, count > 20, dist < 2000)
plot(delay~dist,data=delay)

library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() 

####

destinations <- group_by(flights, dest)
s=summarise(destinations,
            planes = n_distinct(tailnum),
            flights = n()
)
s
arrange(s,desc(flights))
arrange(s,desc(planes))

###########

daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))


##########


a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

a4
plot(a4$dep,a4$arr)
# to samo

filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)

# operator %>%      x %>% f(y) <=> f(x,y)  

flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)-> aa4

plot(arr~dep,data=aa4)


# wlasne funkcje

f=function(x,k=1) x[k,]

flights %>% f(5:8) 

f_max=function(x,var){
  x=as.data.frame(x)
  K=which(names(x)==var)
  x[which.max(x[,K]), ]
}

flights %>% f_max("distance")

flights %>% slice_max(distance) ->a

# Zadania do danych diamonds

library(ggplot2)
data(diamonds)
head(diamonds)

# Prosze uzywac tylko dplyr. 

# Zadania do danych diamonds
#Zad.1 Ile srednio wazyl diament o cut==Ideal?
tmp <- diamonds[diamonds$cut == "Ideal", ]
head(tmp)
tail(tmp)
mean(diamonds$carat)
#Zad.2 Ile jest diamentow o colour==D?
sum(diamonds$color == "D")
#Zad.3 Narysuj rysunek price~carat dla diamentow o price<1000$.
tmp <- diamonds[diamonds$price < 1000, ]
tail(tmp)
plot(tmp$price, tmp$carat)
#Zad.4 Ile diamentow mialo cene wieksza od 10000$?
length(filter(diamonds, price > 10000))
#Zad.5 Podaj srednia cene diamentow dla wszystkich rodzajow cut.
qualities <- diamonds[!duplicated(diamonds$cut),]$cut
qualities
# for (x in qualities) {
  tmp <- diamonds[diamonds$cut == x, ]
  print(mean(tmp$price))
}
#Zad.6 Podaj mediane ceny dla diamentow o wadze powyzej 3 karaty.
tmp <- filter(diamonds, carat > 3)
median(tmp$price)
#Zad.7 Podaj mediane ceny dla diamentow o najlepszej clarity.
clarities <- diamonds[!duplicated(diamonds$clarity),]$clarity
clarities
tmp <- filter(diamonds, clarity == "IF")
head(tmp)
mean(tmp$price)
# Zadania do danych flights
summary(flights)
head(flights)
# Zad. 8 Narysuj rysunek arr_delay~dep_delay dla lot?w z 
#      JFK z 3 kwietnia. 
tmp <- filter(flights, origin == "JFK" & month == 4 & day == 3)
head(tmp)
plot(tmp$arr_delay, tmp$dep_delay)
# Zad. 9 Ile by?o lot?w z Chicago do Cleveland?
length(filter(flights, origin == "Chicago" & dest == "Cleveland"))
# Zad. 10 Wypisz 10 najd?u?szych i 10 najkrotszych lotow.
tmp <- flights[order(flights$distance), ]
head(tmp, n=10)
tail(tmp, n=10)
# Zad. 11 Narysuj rysunek arr_delay~distance dla 100-u najdluzszych lotow. 
tmp <- flights[order(flights[flights$air_time != NA, ]$air_time), ]
tmp2 <- tail(tmp, n=100)
plot(tmp2$arr_delay, tmp2$distance)
# Zad. 12  Jaki byla srednia wartosc arr_delay i distance dla 200-u 
#         najdluzszych lotow?
tmp2 <- tail(tmp, n=200)
mean(tmp2$arr_delay)
mean(tmp2$distance)
# Zad. 13 Przeprowadzic inne (ciekawe) operacje na danych 
#         diamonds lub flights i wykonac rysunek. 


summary_stats <- diamonds %>%
  group_by(cut) %>%
  summarise(
    mean_carat = mean(carat),
    carat_lower = quantile(carat, 0.025),
    carat_upper = quantile(carat, 0.975),
    mean_price = mean(price),
    ci_lower = quantile(price, 0.025),
    ci_upper = quantile(price, 0.975)
  )

ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.3, size = 0.2) +
  
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 10),
              se = TRUE, color = "black", 
              linetype = "dashed", linewidth = 0.8,
              inherit.aes = FALSE, aes(x = carat, y = price)) +
  
  geom_linerange(data = summary_stats,
                 aes(xmin = carat_lower, xmax = carat_upper,
                     y = mean_price, color = cut),
                 linewidth = 0.5,
                 inherit.aes = FALSE) +
  
  geom_pointrange(data = summary_stats,
                  aes(x = mean_carat, y = mean_price,
                      ymin = ci_lower, ymax = ci_upper, color = cut),
                  size = 0.3, linewidth = 0.5,
                  inherit.aes = FALSE) +
  
  coord_cartesian(xlim = range(diamonds$carat), ylim = range(diamonds$price)) +
  
  labs(title = "Diamond Price vs Carat by Cut Quality",
       x = "Carat",
       y = "Price ($)",
       color = "Cut Quality") +
  theme_minimal() +
  theme(legend.position = "right")
