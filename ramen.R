knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)


ramen <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

attach(ramen)

# results='asis' stops knitr from processing the output any further
'Total number of missing values in Ramen data set:' %>% paste(sum(is.na(ramen))) %>% print()  
#print('Missing values by variable:')
#ramen %>% is.na() %>% apply( .,2,sum) %>% print()
ramen %>% is.na() %>% apply( .,2,sum) %>% kable(caption="Missing values by variable:") %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left") %>% column_spec(.,1:2, width = c("5em", "3em"))

# General statistics
'Countries considered:' %>% paste(.,  n_distinct(country)) %>% print()
'Number of distinct styles:' %>% paste(.,  n_distinct(style)) %>% print()
'Number of distinct brands:' %>% paste(.,  n_distinct(brand)) %>% print()
stars %>% mean(na.rm=TRUE) %>% round(1) %>% paste('Average stars: ', .) %>% print()
hist(stars)

# by brand
#print('Varieties of ramen by brand:')
brand.variety <- ramen %>% group_by(brand) %>% summarise(Freq=n())
#print(brand.variety)
brand.variety$Freq %>% mean(na.rm=TRUE) %>% round() %>% paste('Average number of varieties by brand',.) %>% print()

# by style
style.freqs <- ramen %>% group_by(style) %>% summarise(Freq=n(), Rel.freq=round(n()/nrow(ramen)*100,1), Avg=round(mean(stars, na.rm=TRUE),1), SD=round(sd(stars, na.rm=TRUE),2)) %>% arrange(desc(Freq)) 
style.freqs %>% kable(caption='Average stars by style') %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
stars5 <- filter(ramen,stars==5) %>% group_by(style) %>% summarise(Freq5=n())
freqsVS5.style <- select(style.freqs, style, Freq) %>% full_join(stars5, by="style") %>% mutate("%"=round(Freq5/Freq*100,1))
freqsVS5.style %>% kable(caption = 'Ramen rated with 5 stars by style and % vs cases within style') %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")


# By country
country.freqs <- ramen %>% group_by(country) %>% summarise(Freq=n()) %>% arrange(desc(Freq)) %>% mutate(Rel.freq=round(Freq/sum(Freq)*100,1)) %>% mutate(Cum.rel.freq=cumsum(Rel.freq))

short.country.freqs <- filter(country.freqs, Rel.freq>10) 
short.country.freqs <- rbind(short.country.freqs, c('Other', sum(country.freqs$Freq)-sum(short.country.freqs$Freq),100-sum(short.country.freqs$Rel.freq),100))
short.country.freqs %>% kable(caption='Ramen rated by country') %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
lbls <- paste(short.country.freqs$country, '\n', short.country.freqs$Rel.freq, sep="")
pie(as.numeric(short.country.freqs$Rel.freq), 
    labels=lbls,
    main='Ramen rated by country')

top.countries <- filter(ramen, country %in% c('Japan', 'United States', 'South Korea', 'Taiwan')) %>% group_by(country)
summarise(top.countries, Avg=round(mean(stars, na.rm=TRUE),1), SD=round(sd(stars, na.rm=TRUE),2)) %>% kable(caption='Average stars by country') %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# top.countries by style 
top.countries.style <- filter(ramen, country %in% c('Japan', 'United States', 'South Korea', 'Taiwan')) %>% filter(., style %in% c('Pack', 'Bowl', 'Cup', 'Tray', 'Box')) %>% group_by(country, style)
sum.top.countries.style <- summarise(top.countries.style, Freq=n() ,Avg=round(mean(stars, na.rm=TRUE),1), SD=round(sd(stars, na.rm=TRUE),2))
#sum.top.countries.style %>% kable(caption='Average stars by country and style') %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# top.countries by style with stars==5
top.countries.style.5 <- filter(ramen, country %in% c('Japan', 'United States', 'South Korea', 'Taiwan')) %>% filter(., style %in% c('Pack', 'Bowl', 'Cup', 'Tray', 'Box')) %>% filter(., stars==5) %>%group_by(country, style)
sum.top.countries.style.5 <- summarise(top.countries.style.5, Freq5=n())
#sum.top.countries.style.5 %>% kable(caption='Varieties with 5 stars by country and style') %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# merging
freqsVS5.country.style <- sum.top.countries.style %>% full_join(sum.top.countries.style.5, by=c("country","style")) %>% mutate("%"=round(Freq5/Freq*100,1))
freqsVS5.country.style %>% kable(caption='Varieties by country and style: Freq5=varieties rated with 5 stars, % 5 stars vs cases within style') %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# Varieties with 5 stars for ramen in box in Japan
filter(ramen, country=="Japan", style=="Box", stars==5) %>% kable(caption="Ramen in box in Japan with 5 stars") %>% kable_styling(bootstrap_options = "striped", full_width = F, position = "left")
