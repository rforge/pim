#########################
# STANDARD FITTING

# Prepare data

#Some models
data(FEVData)
Model <- pim(FEV~ Smoke*Sex , data=FEVData)

thesummary <- summary(Model)

thesummary[,2:3]

thesummary["Sex"]

thesummary[,2,drop = FALSE]

coef(Model)

confint(Model)
confint(thesummary,
        parm = c("Sex", "Smoke"),
        level = 0.9)

vcov(Model)
fitted(Model)
response(Model)
model.matrix(Model)
has.intercept(Model)
poset(Model)

# More advanced
formula(Model) # gives the pim formula
formula(formula(Model)) # gives a standard formula

PimEnv <- penv(Model) # The pim environment of the model
PimEnv
nobs(PimEnv)
poset(PimEnv)


Model2 <- pim(FEV ~ Age +1, data=FEVData,
              compare="all")

summary(Model2)


Model3 <- pim(PO(L(Height),R(Height)) ~ I(R(Age) - L(Age)) + I(R(Sex) - L(Sex)),
          data=FEVData, 
          estim = "estimator.BB")
# estim is a parameter of pim.fit, see ?pim.fit

summary(Model3)

data(DysData)
Model4 <- pim(P(L(out) != R(out)) ~ I(L(Chemo) != R(Chemo)),
          data=DysData,
          compare="all")
# This is probably too weird, but it works anyway.


#################
# Manual construction
data("FEVData")
# Create the "model frame"
FEVenv <- new.pim.env(FEVData, compare="unique")
# This includes the poset
pos <- poset(FEVenv)

# create the formula and bind it to the pim.environment.
FEVform <- new.pim.formula(
  Age ~ I(L(Height) - R(Height))  ,
  FEVenv
  )

# Use this formula object to construct the model matrix
MM <- model.matrix(FEVform)

# Use this formula object to construct the pseudo response
Y <- response(FEVform)

# Now pim.fit can do what it does
res <- pim.fit(MM,Y, estim = "estimator.glm", penv=FEVenv)

# How to change the score function:
# Obviously this doesn't make any sense, but it's just an illustration.
myscore <- function(x,y,link){
  scorefun <- function(beta){
    Zbeta <- y - x %*% beta
    colSums(Zbeta)
  }
}

# For a bit more guidance, take a look at 
?sandwich.vcov # info on the arguments and output any vcov estimator should 
               # use.
?CreateScoreFun
?estimators

res <- pim.fit(MM,Y,construct = myscore,
               estim = 'estimator.nleqslv',
               vcov.estim="sandwich.vcov",
               penv = FEVenv)

str(res)
coef(res)
vcov(res)
fitted(res)

