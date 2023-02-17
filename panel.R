library(plm)

df.panel.raw <- read.csv("panel.csv", sep = ',', dec = '.', header = TRUE, 
                     stringsAsFactors = FALSE)

# convert to panel data frame
df.panel <- pdata.frame(df.panel.raw, index=c("FIRM","YEAR"))

# pooling
model.pooling <- plm(Y ~ X, data = df.panel, model="pooling")
summary(model.pooling)

# firm/individual  fixed effects
model.firm <- plm(Y ~ X, data = df.panel, model="within", effect="individual")
summary(model.firm)

# first difference
model.fd <- plm(Y ~ X, data = df.panel, model="fd")
summary(model.fd)

# by hand
model.fd <- plm(diff(Y) ~ diff(X), data = df.panel, model="pooling") # 'pooling' has to be specified since 'within' is default
summary(model.fd)

# using function 'lag' to compute differences
df.panel[,"dY"] = df.panel[,"Y"] - lag(df.panel[,"Y"])
df.panel[,"dX"] = df.panel[,"X"] - lag(df.panel[,"X"])
model.fd <- plm(dY ~ dX, data = df.panel, model="pooling") # 'pooling' has to be specified since 'within' is default
summary(model.fd)

# year as regressor
model.year <- lm(Y ~ X + YEAR, data = df.panel.raw)
summary(model.year)

# year fixed effects
model.time <- plm(Y ~ X, data = df.panel, model="within", effect="time")
summary(model.time)
t=lm(Y~ X + as.factor(YEAR),  data = df.panel)
t

# (combined) firm and year fixed effects
model.firm.year <- plm(Y ~ X, data = df.panel, model="within", effect="twoways")
summary(model.firm.year)