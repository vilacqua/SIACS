## Version 0920 (September 2020)

## functions for calculating reaction rate constants.

## the code here is based on KPP output saprc99_m_Rates.m

## ---  Arrhenius
ARR <- function(A0, B0, C0, temp, cfactor) {
  rate <- (A0) * exp(-(B0) / temp) * (temp / 300.0)^(C0)
  return(rate)
} # ARR <- function (...

## --- Simplified Arrhenius, with two arguments
## --- Note: The argument B0 has a changed sign when compared to ARR
ARR2 <- function(A0, B0, temp, cfactor) {
  rate <- (A0) * exp((B0) / temp)
  return(rate)
} # ARR2 <- function (...

## ---
EP2 <- function(A0, C0, A2, C2, A3, C3, temp, cfactor) {
  K0 <- (A0) * exp(-C0 / temp)
  K2 <- (A2) * exp(-C2 / temp)
  K3 <- (A3) * exp(-C3 / temp)
  K3 <- K3 * cfactor * 1.0e+6
  rate <- K0 + K3 / (1.0 + K3 / K2)
  return(rate)
} # EP2 <- function (...

## ---
EP3 <- function(A1, C1, A2, C2, temp, cfactor) {
  K1 <- (A1) * exp(-(C1) / temp)
  K2 <- (A2) * exp(-(C2) / temp)
  rate <- K1 + K2 * (1.0e+6 * cfactor)
  return(rate)
} # EP3 <- function (...

## ---
FALL <- function(A0, B0, C0, A1, B1, C1, CF, temp, cfactor) {
  K0 <- A0 * exp(-B0 / temp) * (temp / 300.0)^(C0)
  K1 <- A1 * exp(-B1 / temp) * (temp / 300.0)^(C1)
  K0 <- K0 * cfactor * 1.0e+6
  K1 <- K0 / K1
  rate <- (K0 / (1.0 + K1)) * (CF)^(1.0 / (1.0 + (log10(K1))^2)) # shc change log to log10
  return(rate)
} # FALL <- function (...
