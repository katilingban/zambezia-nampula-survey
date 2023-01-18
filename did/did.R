rot13 <- function(x)
{
  ## Rotate each letter of a string 'x' by 13 letters thru the alphabet (ROT13 ENDCODING)
  ## Wee use this with abbreviate() to creat plot file names from long insdicatorr names
  old <- paste(letters, LETTERS, collapse="", sep="")
  new <- paste(substr(old, 27, 52), substr(old, 1, 26), sep="")
  chartr(old, new, x)
}

## Retrieve BASELINE and ENDLINE results
baseline <- read.table(file = "outputs/baseline_results_subset.csv", sep = ",", header = TRUE)
endline <- read.table(file = "outputs/endline_results_subset.csv", sep = ",", header = TRUE)
## Make unique indicator IDs
baseline$UIID <- paste(baseline$indicator_set, " - ", baseline$indicator, " - ", baseline$indicator_category, sep = "")
endline$UIID <- paste(endline$indicator_set, " - ", endline$indicator, " - ", endline$indicator_category, sep = "")


## Open text fils for output
sink("did/analysisDiD.html")

## indicators for DiD anaLysis
indicators <- unique(baseline$UIID)
for(i in indicators)
{
  ## COMPARISON group BEFORE
  x <- subset(baseline, study_group == "Comparison" & UIID == i & strata =="Overall")[1,]
  comparison.before <- list(estimate = x$estimate, se = (x$ucl - x$lcl) / (2 * 1.96))
  
  ## Get labels (and plot file name) for output
  labelSet <- x$indicator_set
  labelIndicator <- x$indicator
  ## plotFileName from rot13 encoding of abbrevted 'indiator'
  plotFileName <- paste(abbreviate(rot13(gsub(':', '', i)), minlength = 8, strict = TRUE), "png", sep = ".")
  
  ## COMPARISON group AFTER
  y <- subset(endline, study_group == "Comparison" & UIID == i & strata =="Overall")[1,]
  comparison.after <- list(estimate = y$estimate, se = (y$ucl - y$lcl) / (2 * 1.96))
  
  ## Difference in COMPARISON group AFTER - BEFORE (D1)
  D1 <- comparison.after$estimate - comparison.before$estimate
  se.D1 <- sqrt(comparison.before$se^2 + comparison.after$se^2)
  
  ## TREATMENT group BEFORE
  x <- subset(baseline, study_group == "Intervention" & UIID == i & strata =="Overall")[1,]
  treatment.before <- list(estimate = x$estimate, se = (x$ucl - x$lcl) / (2 * 1.96))
  
  ## TREATMENT group AFTER
  y <- subset(endline, study_group == "Intervention" & UIID == i & strata =="Overall")[1,]
  treatment.after <- list(estimate = y$estimate, se = (y$ucl - y$lcl) / (2 * 1.96))
  
  ## Difference in INTERVENTION groups (D2)
  D2 <- treatment.after$estimate - treatment.before$estimate
  se.D2 <- sqrt(treatment.after$se^2 + comparison.after$se^2)
  
  ## Difference in differences
  DiD = D2 - D1
  se.DiD <- sqrt(se.D2^2 + se.D1^2)
  
  ## 95% confidence limits for DID
  DiD.LCL = DiD - 1.96 * se.DiD
  DiD.UCL = DiD + 1.96 * se.DiD
  
  ## z-test
  z <- abs(DiD / se.DiD)
  z.p <-pnorm(z, lower.tail = FALSE)
  
  cat("<pre>\n\n", labelSet, " : ", labelIndicator, "\n\n", sep = "")
  cat("COMPARISON :            Before\t:", " Estimate = ", round(comparison.before$estimate, 4), "\tSE =", round(comparison.before$se, 4), "\n", sep="")
  cat("COMPARISON :             After\t:", " Estimate = ", round(comparison.after$estimate, 4),  "\tSE =", round(comparison.after$se, 4), "\n", sep="")
  cat("COMPARISON difference     (D1)\t:", " Estimate = ", round(D1, 4), "\tSE =", round(se.D1,4), "\n\n", sep="")
  cat("TREATMENT  :            Before\t:", " Estimate = ", round(treatment.before$estimate, 4),   "\tSE =", round(treatment.before$se, 4), "\n", sep="")
  cat("TREATMWENT :             After\t:", " Estimate = ", round(treatment.after$estimate, 4),    "\tSE =", round(treatment.after$se, 4), "\n", sep="")
  cat("TREATMENT  :   difference (D2)\t:", " Estimate = ", round(D2, 4),"\tSE =", round(se.D2,4), "\n\n", sep="")
  cat("Diference in differences\t:", " Estimate = ", round(DiD,4), "\tSE =", round(se.DiD, 4),                  "\n", sep="")
  cat("                        \t95% CI = [", round(DiD.LCL, 4), ", ", round(DiD.UCL, 4), "]\n", sep = "")
  cat("                   z-test\t:", " z =", round(z, 4), "\tp =", round(z.p, 4), "\n\n", sep = "")
  cat("plot in file  : ", plotFileName, "</pre>\n", sep = "")
  ## Create empty base plotting area
  png(filename = paste0("did/", plotFileName), width = 6, height = 6, units = "in", res = 300, pointsize = 10)
  par(cex.main = 0.67); par(cex.lab = 0.67)
  plot(x = c(1,2), y = c(round(comparison.before$estimate), round(comparison.after$estimate)), type = "n",
       main = paste(strwrap(labelIndicator, width = 50), collapse = "\n"),
       xlab = "", ylab = "Estimate", xaxt = "n", frame.plot = FALSE,
       #	ylim = c(min(comparison.before$estimate, comparison.after$estimate, treatment.before$estimate + D1), max(comparison.before$estimate, comparison.after$estimate, treatment.before$estimate + D1))
  )
  axis(side = 1, at = c(1,2), labels = c("Baseline", "Endline") )
  ## plot path for comparison group
  lines(x = c(1,2), y = c(comparison.before$estimate, comparison.after$estimate), col = "green")
  ## plot path for intrevention group
  lines(x = c(1,2), y = c(treatment.before$estimate, treatment.after$estimate), col = "red")
  ## Plot imagined path for counterfactual
  lines(x = c(1,2), y = c(treatment.before$estimate, treatment.before$estimate + D1), col = "black", lty = 4)
  ## Show treatment effect (DiD)
  arrows(x0 = 2, x1 = 2, y0 = treatment.before$estimate + D1, y1 = treatment.after$estimate, length = 0.1, col = "orange")
  ## plot legend
  legend(x = "topleft", lty = c(1, 1, 4, 1), col = c("green", "red", "black", "orange"), legend = c("Comparison", "Treatment", "Counterfactual", "Treatment effect (DiD)"), cex = 0.75, bty = "n", y.intersp = 1.5)
  ## close graphics device and output text file
  dev.off()
  cat("<img src=\"", plotFileName, "\"", ">", "\n", sep = "")
  flush.console()
}
sink()


## TO DO ... text wrapper fro output, Did Graphic, clean up workspace.
