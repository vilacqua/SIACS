## Version 0922 (September 2022)

Multi.results.plot <- function(out, species2plot, maxTime, parms) {
  OutdoorConcs <- parms$OC
  species.df <- parms$sdf
  # Creates multiple plots on a grid

  ## separate out the indoor and outdoor variables into two matrices
  OutdoorVarNames <- c(
    names(OutdoorConcs),
    "ALK.O", "ARO.O", "OLE.O", "NOy.O", "RO2.O", "HOx.O",
    "To"
  )
  OutdoorOutput <- out[, c("time", OutdoorVarNames)]
  IndoorOutput <- out[, !colnames(out) %in% OutdoorVarNames]
  ## strip ".O" from variable name
  tmp <- unlist(strsplit(OutdoorVarNames, "\\.")) %in% "O"
  colnames(OutdoorOutput) <- c("time", unlist(strsplit(OutdoorVarNames, "\\."))[!tmp])
  ## change To and Ti to T
  colnames(OutdoorOutput)[colnames(OutdoorOutput) == "To"] <- "T" # change "To" to "T"
  colnames(IndoorOutput)[colnames(IndoorOutput) == "Ti"] <- "T" # change "Ti" to "T"
  ## ---------------------------------------------------------------------
  ## some plotting
  ## (two lines per axis, one line for indoor, one line for outdoor)

  gasFlags <- !species2plot %in% species.df$spcname[species.df$gas == 0]
  # match the species above that are not defined as PM
  # this assumes all species (incl. the combined ones not part of spcnames) are gas unless otherwise specified
  others2plot <- c("RH", "LightFlux", "T", "a")
  # other variables to plot
  nskip <- 0
  if (max(out[, "time"]) < maxTime) nskip <- 10
  # no. of timesteps to drop at
  # the end from plotting
  nsteps2plot <- dim(out)[1] - nskip
  # no. of timesteps to plot

  ## extract outdoor concentrations that will be plotted
  tmpVar <- species2plot[species2plot %in% colnames(OutdoorOutput)]
  isgas <- gasFlags[species2plot %in% colnames(OutdoorOutput)]
  OutdoorOutput.df <- as.data.frame(subset(OutdoorOutput[1:nsteps2plot, ],
    select = c("time", tmpVar)
  ))

  OutdoorOutput.df[, which(isgas == 1) + 1] <-
    OutdoorOutput.df[, which(isgas == 1) + 1] * 1000
  # convert from ppm to ppb for gas
  OutdoorOutput.df$type <- "outdoor"
  MeltedOutdoorOutput.df <- melt(OutdoorOutput.df, id = c("time", "type"))

  ## extract indoor concentrations that will be plotted
  tmpVar <- species2plot[species2plot %in% colnames(IndoorOutput)]
  isgas <- gasFlags[species2plot %in% colnames(OutdoorOutput)]
  IndoorOutput.df <-
    as.data.frame(subset(IndoorOutput[1:nsteps2plot, ],
      select = c("time", tmpVar)
    ))
  IndoorOutput.df[, which(isgas == 1) + 1] <-
    IndoorOutput.df[, which(isgas == 1) + 1] * 1000
  # convert from ppm to for gas
  IndoorOutput.df$type <- "indoor"
  MeltedIndoorOutput.df <- melt(IndoorOutput.df, id = c("time", "type"))

  ## combine outdoor and indoor concentrations that will be plotted
  data2plot.df <- rbind(MeltedIndoorOutput.df, MeltedOutdoorOutput.df)

  ## add non-concentration variables that will be plotted)
  tmpVar <- others2plot[others2plot %in% colnames(OutdoorOutput)]
  if (length(tmpVar) > 0) {
    tmp.df <- as.data.frame(subset(OutdoorOutput[1:nsteps2plot, ], select = c("time", tmpVar)))
    tmp.df$type <- "outdoor"
    tmp.df <- melt(tmp.df, id = c("time", "type"))
    data2plot.df <- rbind(data2plot.df, tmp.df)
  }
  tmpVar <- others2plot[others2plot %in% colnames(IndoorOutput)]
  if (length(tmpVar) > 0) {
    tmp.df <- as.data.frame(subset(IndoorOutput[1:nsteps2plot, ], select = c("time", tmpVar)))
    tmp.df$type <- "indoor"
    tmp.df <- melt(tmp.df, id = c("time", "type"))
    data2plot.df <- rbind(data2plot.df, tmp.df)
  }

  ## finally, make the plot
  plot1 <- ggplot(data2plot.df, aes(x = time / 60, y = value, col = type)) +
    geom_line(lwd = 1.3)
  plot1 <- plot1 + facet_wrap(~variable, scales = "free") # so each variable is on its own plot.
  plot1 <- plot1 + xlab("Time (hours)")
  plot1 <- plot1 + ylab(expression(paste("Concentrations (ppb for gas; " ~ mu, "g m"^-3, " for PM; or others)", "")))
  plot1 <- plot1 + expand_limits(y = 0) # so ymin is always zero
  plot1 <- plot1 + labs(col = "", size = 12)
  plot1 <- plot1 + theme_bw()
  plot1 <- plot1 + theme(strip.text.x = element_text(size = 12, colour = "black", angle = 0))
  plot1 <- plot1 + theme(legend.text = element_text(size = 14))
  show(plot1)
} # Multi.results.plot <- function()
