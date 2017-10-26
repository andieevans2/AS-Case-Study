library(lme4)
data=read.csv("case study 2 excel.csv", header = TRUE)
summary(data)
sd(data$flexBefore)
sd(data$rotBefore)
sd(data$difflex)
sd(data$diffrot)
boxplot(difflex~ group + hip, ylab = "difflex", main="Flexion Score Improvements", names=c("control right","treatment right","control left","treatment left"), data)
boxplot(diffrot~ group + hip, ylab = "diffrot", main="Rotation Score Improvements", names=c("control right","treatment right","control left","treatment left"), data)
boxplot(flexBefore ~ group + hip, ylab = "flexLBefore", main="Before Flexion Score", names=c("control right","treatment right","control left","treatment left"),data)
boxplot(rotBefore ~ group + hip, ylab = "rotBefore", main="Before Rotation Score", names=c("control right","treatment right","control left","treatment left"),data)
pairs(data, main = "Scatterplot Matrix")
plot(data$difflex,data$diffrot, xlab = "diff lex", ylab = "diff rot", main = "Scatterplot Matrix for response variables")
ICCbare(subject, difflex, data)
ICCbare(subject, diffrot, data)
ggscatter(data, x = "flexLBefore", y = "flexRBefore",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",)
res <- cor.test(data$flexLBefore, data$flexRBefore, method = "pearson")
res
ggscatter(data, x = "rotLBefore", y = "rotRBefore",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",)
res1 <- cor.test(data$rotLBefore, data$rotRBefore, method = "pearson")
res1
 library(readxl)
 book <- read_excel("C:/Users/Dan J/Desktop/470/Case 2/case/book.xlsx")
 View(book)

model1 = lmer(diffflex~ as.factor(group) + as.factor(hip) + (1|subject) + flexBefore + rotBefore, data = book)
model2 = lmer(diffflex~ as.factor(group) + as.factor(hip) + (1|subject) + flexBefore, data = book)

model.1 = lmer(diffflex~ as.factor(group) + as.factor(hip) + (1|subject) + flexBefore + rotBefore, data = book, REML= FALSE)
model.2 = lmer(diffflex~as.factor(group) +(1|subject) +flexBefore +rotBefore, data = book, REML = FALSE)
anova(model.1, model.2)

model.3 = lmer(diffrot~as.factor(group) +(1|subject) + as.factor(hip) +flexBefore + rotBefore, data = book, REML = FALSE)
model.4 = lmer(diffrot~as.factor(group) +(1|subject) +flexBefore + rotBefore, data = book, REML = FALSE)
anova(model.3, model.4)

model.5 = lmer(diffflex~ as.factor(group) + as.factor(hip) + (1|subject) + flexBefore, data = book, REML= FALSE)
model.6 = lmer(diffflex~as.factor(group) +(1|subject) +flexBefore, data = book, REML = FALSE)
anova(model.5, model.6)

model.7 = lmer(diffrot~as.factor(group) +(1|subject) + as.factor(hip) + rotBefore, data = book, REML = FALSE)
model.8 = lmer(diffrot~as.factor(group) +(1|subject) + rotBefore, data = book, REML = FALSE)
anova(model.7, model.8)
