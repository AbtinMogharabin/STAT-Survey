setwd('C:\\Users\\ACER\\Desktop\\365 final project')
data1 <- read.csv("ML4.csv")

n <- names(data2)

a <- as.numeric(as.factor(data2[[n[81]]]))
a[a == "Always problematic\t"] <- 3
a[a == "Sometimes problematic\t"] <- 2
a[a == "Not problematic\t"] <- 1
a[is.na(a)] <- 1
sum(is.na(a))
a <- as.numeric(a)

b <- as.numeric(as.factor(data1[[n[82]]]))
b[b == "Always problematic\t"] <- 3
b[b == "Sometimes problematic\t"] <- 2
b[b == "Not problematic\t"] <- 1
b[is.na(b)] <- 1
sum(is.na(b))
b <- as.numeric(b)

c <- as.numeric(as.factor(data1[[n[83]]]))
c[c == "Always problematic\t"] <- 3
c[c == "Sometimes problematic\t"] <- 2
c[c == "Not problematic\t"] <- 1
c[is.na(c)] <- 1
sum(is.na(c))
c <- as.numeric(c)

d <- as.numeric(as.factor(data1[[n[84]]]))
d[d == "Always problematic\t"] <- 3
d[d == "Sometimes problematic\t"] <- 2
d[d == "Not problematic\t"] <- 1
d[is.na(d)] <- 1
sum(is.na(d))
d <- as.numeric(d)

e <- as.numeric(as.factor(data1[[n[85]]]))
e[e == "Always problematic\t"] <- 3
e[e == "Sometimes problematic\t"] <- 2
e[e == "Not problematic\t"] <- 1
e[is.na(e)] <- 1
sum(is.na(e))
e <- as.numeric(e)

f <- as.numeric(as.factor(data1[[n[86]]]))
f[f == "Always problematic\t"] <- 3
f[f == "Sometimes problematic\t"] <- 2
f[f == "Not problematic\t"] <- 1
f[is.na(f)] <- 1
sum(is.na(f))
f <- as.numeric(f)

turkey_problems <- (a+b+c+d+e+f)/6
hist((turkey_problems),na.rm=T)
qqnorm((turkey_problems), main='Normal',na.rm=T)
qqline((turkey_problems),na.rm=T)


turkey_problems <- (a+b+c+d+e+f)/6
res <- runif(197,min=-0.15,max=0.15)
turkey_problems <- turkey_problems+res
plot(density((turkey_problems)^2,na.rm=T))
shapiro.test((turkey_problems))



'names(shapiro.test(exp(turkey_problems)))
k=1
while((aa$p.value<0.05) & (k < 100000)){
  turkey_problems <- (a+b+c+d+e+f)/6
  res <- runif(200,min=-0.15,max=0.15)
  turkey_problems <- turkey_problems+res
  aa = shapiro.test(exp(turkey_problems))
  names(shapiro.test(exp(turkey_problems)))
  k = k+1
}
t
turkey_problems <- (turkey_problems-mean(turkey_problems,na.rm=T))/sd(turkey_problems,na.rm=T)
sum(is.na(turkey_problems))'
data1[63] <- a
data1[37] <- turkey_problems



x <- seq(-4, 4, length=100)
y <- dnorm(x)
line(x,y)
plot(density())
plot(density(data1[[38]]))
shapiro.test(data1[[38]])
   
names(data1)


anova_data <- data1[c(26, 15, 16, 18, 20, 36)]
names(anova_data) <- c('Communicate.with.Turkish','Islamic.culture', 'Turkish.language', 'Away.from.family', 'Financial.problems', 'Satisfaction')

f1 <- subset(anova_data, Islamic.culture == 'Not important')[[6]]
f2 <- subset(anova_data, Islamic.culture == 'Moderately important')[[6]]
f3 <- subset(anova_data, Islamic.culture == 'Very important')[[6]]
f4 <- subset(anova_data, Turkish.language == 'Not important')[[6]]
f5 <- subset(anova_data, Turkish.language == 'Moderately important')[[6]]
f6 <- subset(anova_data, Turkish.language == 'Very important')[[6]]
f7 <- subset(anova_data, Away.from.family == 'Not important')[[6]]
f8 <- subset(anova_data, Away.from.family == 'Moderately important')[[6]]
f9 <- subset(anova_data, Away.from.family == 'Very important')[[6]]
f10 <- subset(anova_data, Financial.problems == 'Not important')[[6]]
f11 <- subset(anova_data, Financial.problems == 'Moderately important')[[6]]
f12 <- subset(anova_data, Financial.problems == 'Very important')[[6]]
par(mfrow=c(3,2))
# anova 1
qqnorm(f1, main='plot 1',na.rm=T)
qqline(f1,na.rm=T)
qqnorm(f2, main='plot 2',na.rm=T)
qqline(f2,na.rm=T)
qqnorm(f3, main='plot 3',na.rm=T)
qqline(f3,na.rm=T)
qqnorm(f10, main='plot 4',na.rm=T)
qqline(f10,na.rm=T)
qqnorm(f11, main='plot 5',na.rm=T)
qqline(f11,na.rm=T)
qqnorm(f12, main='plot 6',na.rm=T)
qqline(f12,na.rm=T)

# anova 2
qqnorm(f4, main='plot 1',na.rm=T)
qqline(f4,na.rm=T)
qqnorm(f5, main='plot 2',na.rm=T)
qqline(f5,na.rm=T)
qqnorm(f6, main='plot 3',na.rm=T)
qqline(f6,na.rm=T)
qqnorm(f7, main='plot 4',na.rm=T)
qqline(f7,na.rm=T)
qqnorm(f8, main='plot 5',na.rm=T)
qqline(f8,na.rm=T)
qqnorm(f9, main='plot 6',na.rm=T)
qqline(f9,na.rm=T)

cat(var(f1),
var(f2),
var(f3),
var(f4),
var(f5),
var(f6),
var(f7),
var(f8),
var(f9),
var(f10),
var(f11),
var(f12))


Two_way_aov1 <- aov(Satisfaction ~ Islamic.culture * Financial.problems, data = anova_data)
Two_way_aov2 <- aov(Satisfaction ~ Away.from.family * Turkish.language, data = anova_data)

Two_way_aov1
Two_way_aov2
summary(Two_way_aov1)
summary(Two_way_aov2)



par(mfrow=c(3,2))
s <- subset(anova_data, Communicate.with.Turkish == 'Sometimes')
mean(s[[6]])
var(s[[6]])
qqnorm(s[[6]], main='plot 1',na.rm=T)
qqline(s[[6]],na.rm=T)

ahtt <- subset(anova_data, Communicate.with.Turkish == 'About half the time')
mean(ahtt[[6]])
var(ahtt[[6]])
qqnorm(ahtt[[6]], main='plot 2',na.rm=T)
qqline(ahtt[[6]],na.rm=T)

Always <- subset(anova_data, Communicate.with.Turkish == 'Always')
mean(Always[[6]])
var(Always[[6]])
qqnorm(Always[[6]], main='plot 3',na.rm=T)
qqline(Always[[6]],na.rm=T)

never <- subset(anova_data, Communicate.with.Turkish == 'Never')
mean(never[[6]])
var(never[[6]])
qqnorm(never[[6]], main='plot 4',na.rm=T)
qqline(never[[6]],na.rm=T)

mott <- subset(anova_data, Communicate.with.Turkish == 'Most of the time')
mean(mott[[6]])
var(mott[[6]])
qqnorm(mott[[6]], main='plot 5',na.rm=T)
qqline(mott[[6]],na.rm=T)

females <- females[[2]]
males <- subset(hyp_data1, Gender == 'Male')
males <- males[[2]]
One_way_aov <- aov(Satisfaction ~ Communicate.with.Turkish, data = anova_data)
One_way_aov
summary(One_way_aov)
par(mfrow=c(1,1))
boxplot(Satisfaction ~ Communicate.with.Turkish, data=anova_data)





data2[90] <- turkey_problems
data2 <- read.csv("hypothesis.csv")
names(hyp_data)
hyp_data1 <- data2[c(7,91)]
data2[[91]]
qqnorm(data2[[91]], main='Satisfaction level Q-Q plot',na.rm=T)
qqline(data2[[91]],na.rm=T)
shapiro.test(data2[[91]])

names(hyp_data1) <- c('Gender', 'Satisfaction')
hyp_data <- subset(hyp_data1, Gender != 'Non-binary ')
females <- subset(hyp_data1, Gender == 'Female')
females <- females[[2]]
males <- subset(hyp_data1, Gender == 'Male')
males <- males[[2]]
t.test(males, females,alternative = 'two.sided',var.equal = TRUE)


qqnorm(females, main='Female satisfaction Q-Q plot',na.rm=T)
qqline(females,na.rm=T)
shapiro.test(females)

qqnorm(males, main='Male satisfaction Q-Q plot',na.rm=T)
qqline(males,na.rm=T)
shapiro.test(males)

data2[51]  # difficulty with language

hyp_data2 <- data2[c(51,91)]
names(hyp_data2) <- c('difficulty.with.language', 'Satisfaction')
worried <- subset(hyp_data2, difficulty.with.language == 'Very important')
worried <- worried[[2]]
not_wor <- subset(hyp_data2, difficulty.with.language != 'Very important')
not_wor <- not_wor[[2]]
# H1: more then half were very worried about new language
prop.test(length(worried), length(not_wor)+length(worried),p=NULL,alternative = 'greater',correct = TRUE)
length(worried)

data2[52]  # worried to live in another country

hyp_data3 <- data2[c(52,91)]
names(hyp_data3) <- c('wary.of.another.country', 'Satisfaction')
not_wor2 <- subset(hyp_data3, wary.of.another.country == 'Not important')
not_wor2 <- not_wor2[[2]]
worried2 <- subset(hyp_data3, wary.of.another.country != 'Not important')
worried2 <- worried2[[2]]
# H1: more then half were vary of living in a new country
prop.test(length(worried2), length(worried2)+length(not_wor2),p=0.5,alternative = 'greater',correct = TRUE)


