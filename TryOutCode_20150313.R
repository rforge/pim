#########################
# STANDARD FITTING

# Prepare data
data(FEVData)
FEV <- within(FEVData,{
  Sex <- factor(Sex, levels=c(0,1),
                labels=c("boy","girl"))
  Smoke <- factor(Smoke, levels=c(0,1),
                  labels=c("No","Yes"))
})

#Some models

Model <- pim(FEV~ Age + Smoke*Sex, data=FEV)

Model2 <- pim(FEV ~ I(L(Age)-R(Age)), data=FEV,
              compare="all")


Model3 <- pim(PO(L(Height),R(Height)) ~ I(R(Age) - L(Age)) + I(L(Sex) - R(Sex)),
          data=FEVData, 
          estim = "estimator.BB")
# estim is a parameter of pim.fit, see ?pim.fit

data(DysData)
Model4 <- pim(P(L(out) != R(out)) ~ I(L(SNP_XRCC1__77) != R(SNP_XRCC1__77))*
            I(L(Chemo) != R(Chemo)),
          data=DysData,
          compare="all")
# This is probably too weird, but it works anyway.

#THIS DOESN'T WORK:
Model2 <- pim(FEV ~ I(L(Age)-R(Age)) + Smoke, data=FEV,
              compare="all")
# Should it work?

#################
# Manual construction

# Create the "model frame"
FEVenv <- new.pim.env(FEV, compare="all")
# This includes the poset
pos <- poset(FEVenv, as.list=TRUE)

# create the formula and bind it to the pim.environment.
FEVform <- new.pim.formula(
  as.formula( Age ~ I(L(Height) - R(Height))),
  FEVenv
  )

# Use this formula object to construct the model matrix
MM <- model.matrix(FEVform)

# Use this formula object to construct the pseudo response
Y <- response(FEVform)

# Now pim.fit can do what it does
pim.fit(MM,Y, estim = "estimator.glm")

