if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("MiguelRodo/projrsimple")

usethis::use_git()

usethis::create_github_token()

gitcreds::gitcreds_set()
library(projrsimple)
projr_init()

airquality

count = integer()
incomplete <-  c()
count = 0
O <- airquality['Ozone']
r <- airquality['Solar.R']
for (i in 1:length(O)){
  #count = count+1
  if (is.na(O[i])){
    incomplete <- c(incomplete,i)  
  }}

count = 0
for (i in 1:length(r)){
  #count = count+1
  if (is.na(r[i])){
    incomplete <- c(incomplete,i)  
  }
}
#q1

incomplete <- airquality[apply(airquality,1, function(row) any(is.na(row))), ]
print(incomplete)
#q2
y <- airquality[['Ozone']]
Air <- airquality[['Temp']]
ozone <- c()

for (i in 1:length(y)){
  if (!is.na(y[i])){
    ozone <- c(ozone,y[i])  
  }
}

mean(Air)
sd(Air)
min(Air)
max(Air)

mean(ozone)
sd(ozone)
min(ozone)
max(ozone)
#Q3
cars
y <- cars[,2]
x <- cbind(1,cars[,1])
B = solve(t(x)%*%x)%*%t(x)%*%y

#Q4
model=lm(dist~speed,data = cars)
summary(model)

model=lm(dist~speed,data = cars)
model$coefficients=
  ifelse(calculated_model_outputs$Coeficients==model$coefficients,'Equal Coeficients','Not Equal Coeficients')
summary(model)
identical(as.numeric(calculated_model_outputs$Coeficients),as.numeric(model$coefficients))
as.numeric(model$coefficients)

#q3 modified

y <- cars[,2]
x <- cars[,1]
df <- 48
func = function(x,y,df){
  x = cbind(1,x)
  Beta = solve(t(x)%*%x)%*%t(x)%*%y 
  residuals = y - x%*%Beta
  residual_var = sum(residuals^2) / df
  std_err = sqrt(diag(residual_var*(solve(t(x)%*%x)))) 
  t_val = Beta/std_err
  p_values = 2 * pt(-abs(t_val), df)
  return(list('Coeficients' = Beta, 'StdEror' = std_err, 'P-val' = p_values))
}
func(x,y,df)
