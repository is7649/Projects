# import data from graduation.csv file
grad_file <- read.csv("graduation.csv")

# view again csv
grad_file

# declare potential predictor variables variables
percent_admitted_g = grad_file$Percent.admitted...total..DRVADM2020.
revenue_g = grad_file$Core.revenues..total.dollars..GASB...DRVF2020.
ratio_g = grad_file$Student.to.faculty.ratio..EF2020D.
retention_rate_g = grad_file$Full.time.fall.2019.cohort..EF2020D.
percent_finaid_g = grad_file$Percent.of.full.time.first.time.undergraduates.awarded.any.financial.aid..SFA1920.
percent_pell_g = grad_file$Percent.of.full.time.first.time.undergraduates.awarded.Pell.grants..SFA1920.
in_state_g = grad_file$Published.in.state.tuition.and.fees.2020.21..IC2020_AY.
out_state_g = grad_file$Published.out.of.state.tuition.and.fees.2020.21..IC2020_AY.
student_service = grad_file$Student.service.expenses.as.a.percent.of.total.core.expenses..GASB...DRVF2020.
act = grad_file$ACT.Composite.75th.percentile.score..ADM2020.
SAT_read = grad_file$SAT.Evidence.Based.Reading.and.Writing.75th.percentile.score..ADM2020.
SAT_math = grad_file$SAT.Math.75th.percentile.score..ADM2020.
total_enrollment = grad_file$Total..enrollment..DRVEF2020.

# response variable 
rate_graduation_g = grad_file$Graduation.rate..total.cohort..DRVGR2020.

# scatter plots
# plot for Graduation Rate vs Total Enrollment
plot(percent_admitted_g, rate_graduation_g, main="Graduation Rate v Total Enrollment", xlab="Total Enrollment", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Total Enrollment
total.lm = lm(rate_graduation_g ~ total_enrollment)
total.res = resid(total.lm)
plot(total_enrollment, total.res, ylab="Residuals", xlab="Total Enrollment", main = "Residuals v Total Enrollment")

# plot for Graduation Rate vs Percent Admitted (keep?)
plot(percent_admitted_g, rate_graduation_g, main="Graduation Rate v Percent of Applicants Admitted", xlab="Percent of Applicants Admitted", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Percent Admittted
admitted_g.lm = lm(rate_graduation_g ~ percent_admitted_g)
admitted_g.res = resid(admitted_g.lm)
plot(percent_admitted_g, admitted_g.res, ylab="Residuals", xlab="Percent Admitted", main = "Residuals v Percent Admitted")

# plot for Graduation Rate vs Revenue (NO)
plot(revenue_g, rate_graduation_g, main="Graduation Rate v Revenue", xlab="Revenue", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Revenue
rev_g.lm = lm(rate_graduation_g ~ revenue_g)
rev_g.res = resid(rev_g.lm)
plot(revenue_g, rev_g.res, ylab="Residuals", xlab="Revenue", main = "Residuals v Revenue")

# plot for Graduation Rate vs Student to Faculty Ratio (NO)
plot(ratio_g, rate_graduation_g, main="Graduation Rate v Student to Faculty Ratio", xlab="Student to Faculty Ratio", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Student to Faculty Ratio
ratio_g.lm = lm(rate_graduation_g ~ ratio_g)
ratio_g.res = resid(ratio_g.lm)
plot(ratio_g, ratio_g.res, ylab="Residuals", xlab="Student to Faculty Ratio", main="Residuals v Student to Faculty Ratio")

# plot for Graduation Rate vs Retention Rate (NOT GOOD FOR MULTIPLE REGRESSION)
plot(retention_rate_g, rate_graduation_g, main="Graduation Rate v Retention Rate", xlab="Retention Rate", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Retention Rate
retention_g.lm = lm(rate_graduation_g ~ retention_rate_g)
retention_g.res = resid(retention_g.lm)
plot(retention_rate_g, retention_g.res, ylab="Residuals", xlab="Retention Rate", main="Residuals v Retention Rate")

# plot for Graduation Rate vs Percent of Students Receiving Any Financial Aid
plot(percent_finaid_g, rate_graduation_g, main="Graduation Rate v Percent of Students Receiving Any Financial Aid", xlab="Percent of Students Receiving Any Financial Aid", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Percent of Students Any Financial Aid
fin_g.lm = lm(rate_graduation_g ~ percent_finaid_g)
fin_g.res = resid(fin_g.lm)
plot(percent_finaid_g, fin_g.res, ylab="Residuals", xlab="Percent of Students Receiving Any Financial Aid", main="Residuals v Percent of Students Receiving Any Financial Aid")

# plot for Graduation Rate vs Percent of Students Receiving Pell Grants
plot(percent_pell_g, rate_graduation_g, main="Graduation Rate v Percent of Students Receiving Pell Grants", xlab="Percent of Students Receiving Pell Grants", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Percent of Students Receiving Pell Grants
pell_g.lm = lm(rate_graduation_g ~ percent_pell_g)
pell_g.res = resid(pell_g.lm)
plot(percent_pell_g, pell_g.res, ylab="Residuals", xlab="Percent of Students Receiving Pell Grants", main="Residuals v Percent of Students Receiving Pell Grants")

# plot for Graduation Rate vs In-State Tuition and Fees
plot(in_state_g, rate_graduation_g, main="Graduation Rate v In-State Tuition and Fees", xlab="In-State Tuition and Fees", ylab="Graduation Rate")
# residual plot for Graduation Rate vs In State Tuition and Fees
ins_g.lm = lm(rate_graduation_g ~ in_state_g)
ins_g.res = resid(ins_g.lm)
plot(in_state_g, ins_g.res, ylab="Residuals", xlab="In State Tuition and Fees", main="Residuals v In State Tuition and Fees")

# plot for Graduation Rate vs Out-of-State Tuition and Fees
plot(out_state_g, rate_graduation_g, main="Graduation Rate v Out-of-State Tuition and Fees", xlab="Out-of-State Tuition and Fees", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Out of State State Tuition and Fees
out_g.lm = lm(rate_graduation_g ~ out_state_g)
out_g.res = resid(out_g.lm)
plot(out_state_g, out_g.res, ylab="Residuals", xlab="Out of State Tuition and Fees", main="Residuals v Out of State Tuition and Fees")

# plot for Graduation Rate vs Student Service Expenses as a Percent of Total Core Expenses (KEEP)
plot(student_service, rate_graduation_g, main="Graduation Rate v Student Service Expenses as a Percent of Total Core Expenses", xlab="Student Service Expenses as a Percent of Total Core Expenses", ylab="Graduation Rate")
# residual plot for Graduation Rate vs Student Service Expenses as a Percent of Total Core Expenses
expense.lm = lm(rate_graduation_g ~ student_service)
expense.res = resid(out_g.lm)
plot(student_service, expense.res, ylab="Residuals", xlab="Student Service Expenses as a Percent of Total Core Expenses", main="Residuals v Student Service Expenses as a Percent of Total Core Expenses")

# plot for Graduation Rate vs ACT Composite 75th Percentile Score (KEEP)
plot(act, rate_graduation_g, main="Graduation Rate v ACT Composite 75th Percentile Score", xlab="ACT Composite 75th Percentile Scores", ylab="Graduation Rate")
# residual plot for Graduation Rate vs ACT Composite 75th Percentile Score
ace.lm = lm(rate_graduation_g ~ act)
ace.res = resid(ace.lm)
plot(act, ace.res, ylab="Residuals", xlab="ACT Composite 75th Percentile Score", main="Residuals v ACT Composite 75th Percentile Score")

# plot for Graduation Rate vs SAT Evidence Based Reading and Writing 75th Percentile Score
plot(SAT_read, rate_graduation_g, main="Graduation Rate v SAT Evidence Based Reading and Writing 75th Percentile Score", xlab="SAT Evidence Based Reading and Writing 75th Percentile Scores", ylab="Graduation Rate")
# residual plot for Graduation Rate vs SAT Evidence Based Reading and Writing 75th Percentile Score
read.lm = lm(rate_graduation_g ~ SAT_read)
read.res = resid(read.lm)
plot(SAT_read, read.res, ylab="Residuals", xlab="SAT Evidence Based Reading and Writing 75th Percentile Scoree", main="Residuals v SAT Evidence Based Reading and Writing 75th Percentile Score")

# plot for Graduation Rate vs SAT Math 75th Percentile Score (keep)
plot(SAT_math, rate_graduation_g, main="Graduation Rate v SAT Math 75th Percentile Score", xlab="SAT Math 75th Percentile Scores", ylab="Graduation Rate")
# residual plot for Graduation Rate vs  SAT Math 75th Percentile Score
math.lm = lm(rate_graduation_g ~ SAT_math)
math.res = resid(math.lm)
plot(SAT_math, math.res, ylab="Residuals", xlab="SAT Math Composite 75th Percentile Score", main="Residuals v SAT Math Composite 75th Percentile Score")


# keep variables whose scatter plots have no shape
# SAT math
# SAT read
# ACT
# student service
# PELL
# percent admitted


# multiple regression model
model_g = lm(rate_graduation_g ~ act + SAT_math + SAT_read+ student_service + percent_pell_g + percent_admitted_g, data=grad_file)
summary(model_g)
plot(model_g)


# residual plot multiple regression
plot(model_g$fitted.values, model_g$residuals, main="Residuals Plot for Graduation Rate Model",
     ylab="Residuals",xlab="Estimated Graduation Rates")

# check normality of residuals
hist(model_g$residuals, xlab="Residuals", main="Histogram of Residuals") # histogram of residuals
