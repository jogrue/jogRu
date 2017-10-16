# Deskriptive Beschreibung der Variablen ausgeben, über Schalter kann zwischen Histogramm und Balkendiagramm gewählt werden und gewählt werden, ob Diagramme als PDF gespeichert werden sollen oder überhaupt gar nicht ausgegeben werden sollen.
descriptives <- function(x, name="", histogram=TRUE, barplot=FALSE, densityplot=TRUE, savePDF=FALSE, noPlot=FALSE) {
  require(Hmisc)
  require(psych)
  print(Hmisc::describe(x))
  print(psych::describe(x))
  # if (noPlot) {} else {
  #   drawDistribution(x, label(x), histogram=histogram, barplot=barplot, densityplot=densityplot, savePDF=savePDF, filename=name)
  # }
}

# Berechnet das ordinale Alpha auf Basis der polychorischen Korrelationsmatrix
ordinal_alpha <- function(x){
  require(psych)
  psych::alpha(polychoric(x)$rho)
}
