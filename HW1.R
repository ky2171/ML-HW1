# Question 1
recursion <- function (n) {
  if (n == 1) {
    return(1) 
  }
  else if (n == 2) {
    return(1)
  }
  else {
    return (recursion (n-1)+recursion (n-2)+ 2*(n-2))
  }
}

print(recursion(36))

# Question 2
binomial <- function (n,m) {
  if (m == 0  |  m == n) {
    return (1)
  }
  else {
    result <- binomial(n-1,m) + binomial (n-1,m-1) 
    return (result)
  }
}

print(binomial(88,44))


# Question 3
gcd <- function (x,y) {
  seql = (2:min(x,y))
  for ( i in seql) {
    if (x %% i==0 && y %% i==0) {
      result1 = i
    }
    i = i+1
  }
  return(result1)
}

x <- 12306
y <- 32148

cat("The Greatest Common Divisor is",gcd(x,y))
cat("The Smallest Common Multiple is", x*y/gcd(x,y))



# Question 4 (a)
WHO <- read.csv("/Users/yankeyu/Desktop/WHO copy.csv")
str(WHO)
summary(WHO)
colSums(is.na(WHO)) >=3


# Question 4 (b)
WHO$Country[which.max(WHO$FertilityRate)]
WHO$Country[which.min(WHO$FertilityRate)]

# Question 4 (c)
GNI_sd <-tapply(WHO$GNI, WHO$Region, sd, na.rm = TRUE)
cat (names(GNI_sd[which.min(GNI_sd)]), min(GNI_sd))


# Question 4 (d)
RichCountry = subset (WHO, GNI>20000)
Mean_CM <- mean(RichCountry$ChildMortality, na.rm = TRUE)

cat("The mean child mortality of the rich countries is",Mean_CM)


# Question 4 (e)
plot(WHO$GNI,WHO$LifeExpectancy,xlab = "Income Level",ylab = "Life Expectancy")