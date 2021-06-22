## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=7, fig.height=4)

## -----------------------------------------------------------------------------
library(regDIF)
head(ida)

## ----results='hide'-----------------------------------------------------------
item.data <- regDIF::ida[,1:6]
pred.data <- regDIF::ida[,7:9]
fit <- regDIF(item.data, pred.data, tau = 2)

## -----------------------------------------------------------------------------
summary(fit)

## ----results='hide'-----------------------------------------------------------
fit2 <- regDIF(item.data, pred.data, num.tau = 10)

## -----------------------------------------------------------------------------
fit2

## -----------------------------------------------------------------------------
summary(fit2)

## -----------------------------------------------------------------------------
plot(fit2)

## -----------------------------------------------------------------------------
fit2$impact

## -----------------------------------------------------------------------------
lapply(fit2$eap, head)

## ----results='hide'-----------------------------------------------------------
fit3 <- regDIF(item.data, pred.data, prox.data = rowSums(item.data))

## -----------------------------------------------------------------------------
summary(fit3)

## ----results='hide'-----------------------------------------------------------
fit_net <- regDIF(item.data, pred.data, prox.data = rowSums(item.data), alpha = .5)

## -----------------------------------------------------------------------------
summary(fit_net)

## ----results='hide'-----------------------------------------------------------
alpha_vec <- seq(.1, 1, .1)
fit_net2 <- replicate(length(alpha_vec), NA, simplify = FALSE)

for(a in seq_along(alpha_vec)) {
  fit_net2[[a]] <- regDIF(item.data, pred.data, prox.data = rowSums(item.data), alpha = alpha_vec[a])
}

## -----------------------------------------------------------------------------
which.min(
  sapply(fit_net2, function(fit) min(fit$bic, na.rm = T))
)

## ----results='hide'-----------------------------------------------------------
fit_grp_mcp <- regDIF(item.data, pred.data, prox.data = rowSums(item.data), pen.type = "grp.mcp")

## -----------------------------------------------------------------------------
summary(fit_grp_mcp)

## ----echo=F-------------------------------------------------------------------
citation("regDIF")

