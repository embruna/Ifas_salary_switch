library(tidyverse)

# rm(list = ls())

####################
#### ADJUST VALUES in 1-4 as needed.
####################

############################################
# 1) BASE SALARY
############################################

Sal.init.12=134000 #initial salary

############################################
# 2) Adjustments to BASE SALARY if switching to 9 mo appt.
############################################

salary.reduction=0 #percent decrease in 12 mo salary if accepting 9 mo appt
raise.yr1.9mo=0 #percent raise in yr 1 if moving to 9 mo
raise.yr2.9mo=0 #percent raise in yr 2 if moving to 9 mo
raise.yr3.9mo=0 #or annual.raise #percent raise in yr 3 if moving to 9 mo. [originally had us going no-raise for 3 yrs]

############################################
# 3) ESTIMATED months of summer salary in Years 1-3 if switching to 9 month appt 
############################################

# If you move to a 9 month appointment, you can try to get summer salary. 
# Here you can modify how many months of salary you get from grants in yrs 1-3 after switching
salary.grant.months.yr1=1 #9 mo only
salary.grant.months.yr2=1 #9 mo only
salary.grant.months.yr3=1 #9 mo only

################################
# 4) ESTIMATES FOR PROJECTIONS OF FUTURE SALARY, SUMMER SALARY, AND INVESTMENTS
################################
annual.raise=2.5 #percent annual raise, estimated

annual.rate=8 #annual percent interest rate of investments, estimated

years_to_project<-20  #for projections of future salary & investment; yr 1 = year 1 of salary switch plan

actual_summer_salary_funded<-1 #how many months of summer salary are you tryying to get each summer in the projections () 

prob=0.25 #prob of actually having this number of months of summer salary in years 4-24


############
# DO NOT MANIPULATE ANY MORE VARIABLES BELOW!!!!
############







################################
################################

# RESULTS / PROJECTIONS

################################
################################


################################
# Your new BASE SALARY if switching to 9 mo 
################################

Sal.1.9=Sal.init.12*(1-(salary.reduction)/100)
Sal.2.9=Sal.1.9*(1+raise.yr2.9mo/100)
Sal.3.9=Sal.2.9*(1+raise.yr3.9mo/100)

################################
# Your new TOTAL SALARY yrs 1-3 if switching to 9 mo
# Calculated as: 
  # new 9 month salary + 
  # summer any summer salary you get (calculated as 1/9 of base salary) + 
  # leave payout (year 1 only)

################################
leave.payout=200*(Sal.init.12/2088) #200 hours; added only to year 1 if converting to 9 mo.
Sal.1.9.with.summer=Sal.1.9+(Sal.1.9/9*salary.grant.months.yr1)+leave.payout # OR SHOULD YOU USE THE ORIGINAL 12 MONTH SALARY? 
Sal.2.9.with.summer=Sal.2.9+(Sal.2.9/9*salary.grant.months.yr2) 
Sal.3.9.with.summer=Sal.3.9+(Sal.3.9/9*salary.grant.months.yr3) 



######################################################################################
## STAY AT 12 MONTH APPOINBTMENT: Salary and investments after 1st 3 years
######################################################################################

# DO NOT CHANGE THIS!
rate=annual.rate/12 #we are compounfing monthly so divide annual rate by 12
n=12 # no of months projecting we are doing this we are doing 3 annual projections to see amount after 36 months 

# Total investment at the end of 36 mos (principal + Interest): retain 12 month salary & raises
# YEAR 1
Sal.1.12=Sal.init.12*(1+annual.raise/100)
P1.12=(Sal.1.12/12)*0.0514 #amount of monthly deposit = monthly salary * percent placed in investment account

# This code creates a vector, prin with the principle at the beginning of each period, 
# and then a vector int containing the interest earned in that period. 
prin1.12 <- P1.12 * (1+rate/100)^(0:(n-1))
int1.12  <- prin1.12 * rate/100
totalInt.1.12 <- sum(int1.12)
Invest.proj.1.12<-totalInt.1.12+sum(prin1.12)

# YEAR 2
Sal.2.12=Sal.1.12*(1+annual.raise/100)
P2.12=(Sal.2.12/12)*0.0514 #amount of monthly deposit = monthly salary * percent placed in investment account

prin2.12 <- (P2.12) * (1+rate/100)^(0:(n-1))
int2.12  <- prin2.12 * rate/100
totalInt.2.12 <- sum(int2.12)

carry.forward.2.12<-Invest.proj.1.12*(1+rate/100)^(0:(n-1))
carry.forward.2.12<-carry.forward.2.12[12]
Invest.proj.2.12<-sum(prin2.12)+totalInt.2.12+carry.forward.2.12

# YEAR 3
Sal.3.12=Sal.2.12*(1+annual.raise/100)
P3.12=(Sal.2.12/12)*0.0514 #amount of monthly deposit = monthly salary * percent placed in investment account

prin3.12 <- (P3.12) * (1+rate/100)^(0:(n-1))
int3.12  <- prin3.12 * rate/100
totalInt.3.12 <- sum(int3.12)

carry.forward.3.12<-Invest.proj.2.12*(1+rate/100)^(0:(n-1))
carry.forward.3.12<-carry.forward.3.12[12]
Invest.proj.3.12<-sum(prin3.12)+totalInt.3.12+carry.forward.3.12


######################################################################################
## SWITCH TO A 9 MONTH  APPOINBTMENT: Salary and investments after 1st 3 years
######################################################################################

# Total investment at the end of 36 mos (principal + Interest): 9 month salary with raises

# YEAR 1
#amount of monthly deposit = monthly salary * percent placed in investment account
P1.9=(Sal.1.9/12)*0.0514 

# This code creates a vector, prin with the principle at the beginning of each period, 
# and then a vector int containing the interest earned in that period. 
prin1.9 <- P1.9 * (1+rate/100)^(0:(n-1))
int1.9  <- prin1.9 * rate/100
totalInt.1.9 <- sum(int1.9)

Invest.proj.1.9<-sum(prin1.9)+totalInt.1.9
Invest.proj.1.9

# YEAR 2
#amount of monthly deposit = monthly salary * percent placed in investment account
P2.9=(Sal.2.9/12)*0.0514 
# This code creates a vector, prin with the principle at the beginning of each period, 
# and then a vector int containing the interest earned in that period. 
prin2.9 <- (P2.9) * (1+rate/100)^(0:(n-1))
int2.9  <- prin2.9 * rate/100
totalInt.2.9 <- sum(int2.9)

carry.forward.2.9<-Invest.proj.1.9*(1+rate/100)^(0:(n-1))
carry.forward.2.9<-carry.forward.2.9[12]
Invest.proj.2.9<-sum(prin2.9)+totalInt.2.9+carry.forward.2.9
Invest.proj.2.9

# YEAR 3
#amount of monthly deposit = monthly salary * percent placed in investment account
P3.9=(Sal.3.9/12)*0.0514 
# This code creates a vector, prin with the principle at the beginning of each period, 
# and then a vector int containing the interest earned in that period. 
prin3.9 <- (P3.9) * (1+rate/100)^(0:(n-1))
int3.9<- prin3.9 * rate/100
totalInt.3.9 <- sum(int3.9)

carry.forward.3.9<-Invest.proj.2.9*(1+rate/100)^(0:(n-1))
carry.forward.3.9<-carry.forward.3.9[12]
Invest.proj.3.9<-sum(prin3.9)+totalInt.3.9+carry.forward.3.9


######################################################################################
######################################################################################
######################################################################################
## SUMMARY OF SALARY AND INVESTMENT after 1st 3 years
######################################################################################
######################################################################################
######################################################################################

######################################################################################
# 12 Month
######################################################################################

Invest.proj.1.12
Invest.proj.2.12
Invest.proj.3.12 #retirement investment after 3 years on 12 mo appt
Sal.1.12 #salary at end of 3 years on 12 mo appt
Sal.2.12
Sal.3.12


######################################################################################
# 9 Month
######################################################################################

Invest.proj.1.9
Invest.proj.2.9
Invest.proj.3.9 #retirement investment after 3 years on 9 mo appt
Sal.1.9
Sal.2.9
Sal.3.9 #salary at end of 3 years on 9 mo appt



######################################################################################
## NOW PROJECT 20 YEARS INTO THE FUTURE: STAY 12 MO
######################################################################################
Base_Salary<-Sal.init.12
carry.forward<-0
#carry.forward<-Invest.proj.1.12

investment_return <- function(x,y,z) {
  data<-as.data.frame(seq(1:years_to_project))
  data$salary<-NA
  # data$carry.forward<-NA
  # data$totalInt.proj<-NA
  data$Invest.proj<-NA
  data$InvestmentReturn<-NA
  data$summer_salary<-NA
  data$total_salary<-NA
  # data$InvestmentReturn_cumulative<-NA
  
  for (i in 1:z) {
    
    
    summer_salary<-Base_Salary/12*rbinom(length(prob), size = 1, prob=0) #QUESTION THIS SHOULD BE 9 or 12??!!!
    actual_summer_salary_funded<-0 #You can calibrate how much you get (i.e., ask for 50% of salary or 100% of salary, etc.)
    total_salary<-Base_Salary+(summer_salary*actual_summer_salary_funded)
    data$total_salary[i]<-total_salary
    Projections=((total_salary)/12)*0.0514 #amount of monthly deposit = monthly salary * percent placed in investment account
    prin.proj <- Projections*(1+rate/100)^(0:(n-1))
    
    carry.forward<-carry.forward*(1+rate/100)^(0:(n-1))
    carry.forward<-carry.forward[12]
    int.proj  <- prin.proj * rate/100
    totalInt.proj <- sum(int.proj)
    Invest.proj<-sum(prin.proj)+carry.forward+sum(int.proj) #Principal+Interest
    data$Invest.proj[i]<-Invest.proj
    data$year[i]
    data$salary[i]<-Base_Salary
    data$summer_salary[i]<-summer_salary
    # data$carry.forward[i]<-carry.forward
    # data$totalInt.proj[i]<-totalInt.proj
    data$InvestmentReturn[i]<-Invest.proj
    Base_Salary<-Base_Salary*(1+annual.raise/100)
    carry.forward<-Invest.proj
  }
  #data[,"InvestmentReturn_cumulative"] <- cumsum(data$InvestmentReturn)
  data[,"BaseSalary_cumulative"] <- cumsum(data$salary)
  data[,"TotalSalary_cumulative"] <- cumsum(data$total_salary)
  data[,"plan"] <- "12month"
  # ReturnList <- list("Projected.Investment.Return" = Invest.proj, "Salary" = Base_Salary.end.yr3)
  return(data)
  # return(ReturnList)
  
}
projections.12mos<-investment_return(Base_Salary,carry.forward,years_to_project)
projections.12mos



######################################################################################
## NOW PROJECT 20 YEARS INTO THE FUTURE: SWITCH TO 9 MO
######################################################################################
years_to_project_adjusted<-years_to_project-3


df91<-c(Sal.1.9,
        Invest.proj.1.9, 
        Invest.proj.1.9,
        (Sal.1.9/9*salary.grant.months.yr1),
        Sal.1.9.with.summer,
        Sal.1.9.with.summer)
df92<-c(Sal.2.9,Invest.proj.2.9,Invest.proj.2.9,(Sal.2.9/9*salary.grant.months.yr2),Sal.2.9.with.summer,Sal.1.9.with.summer+Sal.2.9)
df93<-c(Sal.3.9, 
        Invest.proj.3.9,
        Invest.proj.3.9,
        (Sal.3.9/9*salary.grant.months.yr3),
        Sal.3.9.with.summer,
        Sal.1.9.with.summer+Sal.2.9+Sal.3.9)

yrs1.3_9<-as.data.frame(rbind(df91,df92,df93))
names(yrs1.3_9)<-c("salary", "Invest.proj" ,"InvestmentReturn","summer_salary", "total_salary","TotalSalary_cumulative") 

Base_Salary<-Sal.1.9*(1+annual.raise/100)   #WHAT SHOULD YOU CONSIDER BASE SALARY FOR 9 mos WHEN SIMULATING FORWARD?  
# carry.forward<-Invest.proj.1.9 #toggle off if projections start 20189 if 
carry.forward<-yrs1.3_9$InvestmentReturn[3]
investment_return <- function(x,y,z) {
  data<-as.data.frame(seq(1:years_to_project_adjusted))
  data$salary<-NA
  # data$carry.forward<-NA
  # data$totalInt.proj<-NA
  data$Invest.proj<-NA
  data$InvestmentReturn<-NA
  data$summer_salary<-NA
  data$total_salary<-NA
  #data$InvestmentReturn_cumulative<-NA
  for (i in 1:z) {
    
    summer_salary<-Base_Salary/9*rbinom(length(prob), size = 1, prob=prob) #QUESTION THIS SHOULD BE 9 or 12??!!!
    
     total_salary<-Base_Salary+(summer_salary*actual_summer_salary_funded)
    data$total_salary[i]<-total_salary
    Projections=((total_salary)/12)*0.0514 #amount of monthly deposit = monthly salary * percent placed in investment account
    prin.proj <- Projections*(1+rate/100)^(0:(n-1))
  
    carry.forward<-carry.forward*(1+rate/100)^(0:(n-1))
    carry.forward<-carry.forward[12]
    int.proj  <- prin.proj * rate/100
    
    Invest.proj<-sum(prin.proj)+carry.forward+sum(int.proj) #Principal+Interest
    data$Invest.proj[i]<-Invest.proj
    data$year[i]
    data$salary[i]<-Base_Salary
    data$summer_salary[i]<-summer_salary
    # data$carry.forward[i]<-carry.forward
    # data$totalInt.proj[i]<-totalInt.proj
    data$InvestmentReturn[i]<-Invest.proj
    Base_Salary<-Base_Salary*(1+annual.raise/100)
    carry.forward<-Invest.proj
    
  }
  # data[,"InvestmentReturn_cumulative"] <- cumsum(data$InvestmentReturn)
  # data[,"TotalSalary_cumulative"] <- cumsum(data$salary)
  # data[,"TotalSalary_cumulative"] <- cumsum(data$total_salary)
  # data[,"plan"] <- "9month"
  # # ReturnList <- list("Projected.Investment.Return" = Invest.proj, "Salary" = Base_Salary.end.yr3)
  return(data)
  # return(ReturnList)
  
}
projections.9mos<-investment_return(Base_Salary,carry.forward,years_to_project_adjusted)
projections.9mos

names(projections.9mos)
yrs1.3_9$"seq(1:years_to_project_adjusted)"<-seq(1:3)
yrs1.3_9<-yrs1.3_9 %>% select("seq(1:years_to_project_adjusted)","salary","Invest.proj","InvestmentReturn","summer_salary","total_salary")  
names(yrs1.3_9)
projections.9mos<-rbind(yrs1.3_9,projections.9mos)

projections.9mos$"seq(1:years_to_project_adjusted)"<-seq(1:20)
projections.9mos$InvestmentReturn_cumulative <- cumsum(projections.9mos$InvestmentReturn)
projections.9mos$BaseSalary_cumulative <- cumsum(projections.9mos$salary)
projections.9mos$TotalSalary_cumulative <- cumsum(projections.9mos$total_salary)
projections.9mos$plan<-"9month"

names(yrs1.3_9)
names(projections.9mos)



######################################################################################
## VISUALIZATION YRS 4-24
######################################################################################
names(projections.9mos)
names(projections.12mos)

projections.12mos<-projections.12mos %>% rename("year"="seq(1:years_to_project)") 
projections.9mos<-projections.9mos %>% rename("year"="seq(1:years_to_project_adjusted)") %>% select(-InvestmentReturn_cumulative)
DATA<-bind_rows(projections.12mos,projections.9mos)

DATA$plan<-as.factor(DATA$plan)
str(DATA)


# CUMULATIVE BASE SALARY OVER TIME

plot.proj.cum.base.sal<-ggplot(data=DATA, aes(x=year, y=(BaseSalary_cumulative), group=plan,colour=plan)) +
  geom_line() +
  geom_point() +
  ylab("Base Salary (Cumulative)") + xlab("Year") +
  ggtitle("Cumulative salary, Base (includes raises; no summer salary included)")
plot.proj.cum.base.sal


# CUMULATIVE TOTAL SALARY OVER TIME (Base + Summer)

plot.proj.cum.sal<-ggplot(data=DATA, aes(x=year, y=(TotalSalary_cumulative), group=plan,colour=plan)) +
  geom_line() +
  geom_point() +
  ylab("Total Salary (Cumulative)") + xlab("Year") +
  ggtitle("Cumulative salary, Total (includes raises & summers)")
plot.proj.cum.sal


# CUMULATIVE INVESTMNET TOTAL OVER TIME

plot.proj.invest<-ggplot(data=DATA, aes(x=year, y=(InvestmentReturn), group=plan,colour=plan)) +
  geom_line() +
  geom_point() +
  ylab("Investments (Cumulative)") + xlab("Year") +
  ggtitle("Estimated Investments, Cumulative")
plot.proj.invest




######################################################################################
## SAVE OUTPUT FILE
######################################################################################

write.csv(DATA, file = "Nine_Twelve_Projections_20yrs.csv")

# wide_DATA <- DATA %>% spread(plan,salary)
######################################################################################
## IS T WORTH IT? NET DIFF IN $
######################################################################################
EDIT THIS TO USE  DATA and select row year = 20
NOT SURE IT IS DOING INVESTMENT CUMULATIVE PROPERLY

# stay 12: DIFF IN CUMULATIVE SALARY  
sal.diff.cum<-DATA$TotalSalary_cumulative[years_to_project]-projections.9mos$TotalSalary_cumulative[years_to_project]
sal.diff.cum # negative numbers = lower cumulative salary over 20 years if staying on 12 month

# stay 12: DIFF IN CUMULATIVE INVEST  
inv.diff.cum<-projections.12mos$InvestmentReturn_cumulative[years_to_project]-projections.9mos$InvestmentReturn_cumulative[years_to_project]

# Net if stay 12
net.diff.cum<-sal.diff.cum+inv.diff.cum
net.diff.cum #NEGATIVE VALUES INDICATE "COST" ($ LOSS) OF STAYING AS 12 MONTH
