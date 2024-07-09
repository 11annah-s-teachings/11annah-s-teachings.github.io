
list_of_packages <- c("ggplot2", "HSAUR3", "lattice", "MASS")
new_packages <- list_of_packages[!(list_of_packages %in%
                                     installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
sapply(list_of_packages, require, character.only = TRUE)



library(ggplot2)
theme_set(theme_bw()) # globales ggplot2-Theme festsetzen

# a) ----------------------------------------------------------------------
data("gardenflowers", package = "HSAUR3")
gardenflowers
class(gardenflowers)

library(lattice)
gm <- as.matrix(gardenflowers)

levelplot(gm,
          aspect="iso", scales=list(x=list(rot=90)),
          main="Distance Matrix",
          cuts=100, at=seq(0,1,0.01))


# Erkennt man bereits etwas?
# -> Nicht wirklich. Lediglich die Gruppe von 'Pink carnation' bis 'Heather'
#    sieht einigerma?en ?hnlich aus, d.h. die jeweilige paarweise 
#    Distanz zwischen diesen Blumen scheint eher klein zu sein.



# b) ----------------------------------------------------------------------
# Klassische (metrische) MDS
cmd <- cmdscale(gardenflowers, k = 2, eig = TRUE)

### Betrachtung der kumulierten Eigenwerte zur Bestimmung der Anzahl an Dimensionen
round(cmd$eig, 5)
# -> teilweise negative Eigenwerte -> Betrachtung der absoluten bzw. quadrierten EW
ew_abs <- abs(cmd$eig)
ew_quad <- cmd$eig^2
# Kumulierte (und skalierte) Eigenwerte
cumsum(ew_abs) / sum(ew_abs)
cumsum(ew_quad) / sum(ew_quad)

# -> mit beiden Methoden liegt man deutlich unter alpha = 0.75 fuer die ersten beiden Eigenwerte

# -> In der Praxis wuerde man nun daher nicht mit zwei Dimensionen weiterarbeiten,
#    aus Gruenden der Einfachheit und Visualisierung machen wir das aber trotzdem im Folgenden.

### Daten in 2D plotten
X <- as.data.frame(cmd$points)
# Namen der Arten k?rzen f?r Plot: Nur den englischen Namen vor der Klammer verwenden
names <- unname(sapply(row.names(X), function(name) strsplit(name, split = "\\ \\(")[[1]][1]))
X$name <- names
ggplot(X, aes(x=V1, y=V2)) + 
  geom_text(aes(label = name))

# -> Interpretation: Plot deutet auf 4-5 Gruppen hin, jeweils bestehend aus mind. 3 Arten.



# c) ----------------------------------------------------------------------
# Nichtmetrische MDS
library(MASS)
nmd <- isoMDS(gardenflowers, k = 2)

### Betrachtung des Stresses als Guetekriterium
nmd$stress
# -> Abweichung von knapp 20% ist sehr hoch. Man sieht also auch hier,
#    dass die Beschraenkung auf zwei Dimensionen nicht optimal ist.

# Screeplot des Stresses fuer verschiedene Anzahlen an Dimensionen
model_list <- vector(length = 8, mode = "list")
for(i in 1:8) {
  model_list[[i]] <- isoMDS(gardenflowers, k = i)
}
stresses <- sapply(model_list, function(model) model$stress)
plot(stresses, type = "l", main = "Screeplot\nStress gegen die Anzahl an Dimensionen",
     xlab = "Anzahl Dimensionen", ylab = "Stress [%]")
# -> Screeplot nimmt leider nur recht langsam ab, aber zumindest reduziert eine
# Betrachtung in drei Dimensionen den Stress um 1/3 auf ca. 12%
# --> "zufriedenstellend"


### Daten in 2D plotten
X2 <- as.data.frame(nmd$points)
# Verwende in b) extrahierte kurze Namen
X2$name <- names
ggplot(X2, aes(x=V1, y=V2)) + 
  geom_text(aes(label = name))

# -> Interpretation: Relativ aehnliche Ergebnisse zur metrischen MDS. Die Arten
#    sind hier jedoch etwas verstreuter, wodurch die Anwendung dieser Methode
#    weniger stark auf voneinander stark unterschiedliche Gruppen hinweisen wuerde.



# d) ----------------------------------------------------------------------

### (i)
# -> Jede der beiden extrahierten Dimensionen/Skalen kann man mit den urspruenglichen
#    Variablen korrelieren, auf Basis welcher man die Distanzen berechnet hat.
#    Im Optimalfall gibt es dann nur wenige sehr hohe Korrelationen und viele
#    sehr niedrige Korrelationen, s.d. klar wird, welche Variablen in welche
#    der Dimensionen einfliessen.
# -> Aehnlich zur Interpretation bei der PCA

### (ii)
# -> Gar nicht!
#    Grund: PCA macht eine Eigenwertzerlegung der Kovarianz-/Korrelationsmatrix
#           MDS macht eine Eigenwertzerlegung der Gram-Matrix (Skalarprodukt-Matrix) B
#           -> Im Fall der Verwendung euklidischer Distanzen fuehren die beiden Methoden
#              zu identischen Ergebnissen
#    Beachten: Bei Verwendung eines anderen Distanzmasses fuehrt eine metrische MDS zu anderen Ergebnissen!
