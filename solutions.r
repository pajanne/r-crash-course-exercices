file.exists("gapminder.csv")

gapminder <- read.csv("gapminder.csv")
View(gapminder)

test <- read.delim("gapminder.csv")
head(test)
dim(test)

gapminder$country
summary(gapminder)

# --------------------------------------------------------------------------------

# Save the life expectancy and population as variables 

pop <- gapminder$pop
head(pop)

life_expectancy <- gapminder$lifeExp
head(life_expectancy)

# what is the maximum life expectancy?
max(life_expectancy)

# what is the smallest population?
min(pop)

# round the life expectancy and populations to the nearest whole numbers
round(life_expectancy)

# --------------------------------------------------------------------------------
# Subsetting

gapminder
gapminder[1,2]
gapminder[2,1]
gapminder[c(1,2,3),1]
gapminder[c(1,2,3),c(1,2,3)]
gapminder[1:3,1:2]
gapminder[seq(1,1704,length.out = 10),1:4]
head(gapminder)

gapminder$lifeExp < 40
gapminder[gapminder$lifeExp < 40, ]
gapminder[gapminder$lifeExp < 40, 1:4]
gapminder[gapminder$lifeExp < 40, c("country", "continent","year")]
gapminder[gapminder$country == "Zambia",]
gapminder[grep("land", gapminder$country),]
gapminder[gapminder$country == "Zambia" | gapminder$country == "Zimbabwe",]
gapminder[gapminder$country %in% c("Zambia","Zimbabwe"),]
gapminder[gapminder$country == "Zambia" & gapminder$lifeExp < 40,]

# --------------------------------------------------------------------------------

# create a data frame of countries with a population less than a million in the year 2002
head(gapminder)
gapminder[gapminder$year == "2002" & gapminder$pop < 1000000,]

# A data frame of countries with a population less than a million in the year 2002, that are not in Africa?
gapminder[gapminder$year == "2002" & gapminder$pop < 1000000 & gapminder$continent != 'Africa',]

# --------------------------------------------------------------------------------
# Ordering and sorting

countries <- gapminder$country
sort(countries)
sort(countries, decreasing = TRUE)

leastPop <- gapminder[order(gapminder$pop),]
head(leastPop)

byWealth <- gapminder[order(gapminder$gdpPercap,decreasing = TRUE),]
head(byWealth)
write.csv(byWealth, file="dataOrderedByWealth.csv")
gapminder[order(gapminder$year, gapminder$country),]

# --------------------------------------------------------------------------------
# Plotting and stats (in brief!)

hist(gapminder$lifeExp)

plot(gapminder$pop,gapminder$lifeExp)

barplot(table(gapminder$continent))

boxplot(gapminder$gdpPercap ~ gapminder$continent)

plot(gapminder$pop,gapminder$lifeExp,pch=16,
     col="red",ylab="Life Expectancy",
     xlab="Population",main="Life Expectancy trend with population")

boxplot(gapminder$gdpPercap ~ gapminder$continent,col=c("red","orange","green","blue","purple"),
        main="GDP per-continent",
        xlab="Continent",
        ylab="GDP")

pdf("myLittlePlot.pdf")
barplot(table(gapminder$continent))
dev.off()

# The canvas model

euroData <- gapminder[gapminder$continent == "Europe" & gapminder$year == 2002,]
dim(euroData)

afrData <- gapminder[gapminder$continent == "Africa" & gapminder$year == 2002,]
dim(afrData)

plot(euroData$pop, euroData$lifeExp,col="red",
     pch=16,
     xlab="Population",
     ylab="Life Expectancy")
points(afrData$pop, afrData$lifeExp,col="blue",pch=16)

plot(euroData$pop, euroData$lifeExp,col="red",
     pch=16,
     xlab="Population",
     ylab="Life Expectancy",
     xlim=c(0,8e7),ylim=c(30,90))
points(afrData$pop, afrData$lifeExp,col="blue",pch=16)

library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)

boxplot(gapminder$gdpPercap ~ gapminder$continent,col=brewer.pal(5,"Set1"),
        main="GDP per-continent",
        xlab="Continent",
        ylab="GDP")

# --------------------------------------------------------------------------------
# Statistical Testing
# more on stats http://www.bioinformatics.babraham.ac.uk/training/R_Statistics/Introduction%20to%20Statistics%20with%20R.pdf

x <- rnorm(20)
y <- rnorm(20, 5,1)
df <- data.frame(x,y)
boxplot(df)

t.test(x,y)
t.test(x,y,paired = TRUE)

x <- rnorm(20)
y <- rnorm(20, 5,4)
df <- data.frame(x,y)
boxplot(df)

t.test(x,y,var.equal = FALSE)

# --------------------------------------------------------------------------------
# Towards Reproducibility
# Let’s create a new Markdown file with File -> New File -> R markdown and look at the contents.
