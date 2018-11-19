x = 'Hello'
print(x)

print(getwd())

tail(iris, n=5)
typeof(iris)
mode(iris)
class(iris)
class(iris$Sepal.Length)

install.packages("DT", dependencies = TRUE)
library(DT)
DT::datatable(iris)
DT::datatable(data.summary)
descStats <- function(col) {
  mean <- c(mean(col))
  f <- fivenum(col)
  return(list(m, f))
}

data.input <- iris[,-1]
data.summary <- summary(iris)
print(data.summary)
class(data.summary)


tail(data.input, n=5)

sum <- summary(iris$Sepal.Length)
sum[1]
descStats(iris$Sepal.Length)
colnames(iris[1])
mean(iris$Sepal.Length)
fivenum(iris$Sepal.Length)

system('g++ -v')
library(inline)
library(Rcpp)

src <- ' 
  std::vector<std::string> s; 
  s.push_back("hello");
  s.push_back("world");
  return Rcpp::wrap(s);
  '
hellofun <- cxxfunction(body = src, includes = '', plugin = 'Rcpp', verbose = FALSE)
cat(hellofun(), '\n')

rversions
system('g++ -v')

options(repos = c(getOption("repos"), rstan = "http://wiki.rstan-repo.googlecode.com/git/"))
install.packages('rstan', type = 'source')
