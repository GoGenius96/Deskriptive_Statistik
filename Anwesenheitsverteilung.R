# Particify
# Länge muss 8 sein
analysis <- c(6,1,2,0,2,2,1,44)
statistik <- c(3,0,0,0,2, 4, 2, 49)

X_analysis <- do.call(rep, args = list(x = 0:7, times = analysis))
X_statistik <- do.call(rep, args = list(x = 0:7, times = statistik))

# relative Häufigkeiten
f_analysis <- analysis/sum(analysis)
names(f_analysis) <- 0:7
f_analysis

f_statistik <- statistik/sum(statistik)
names(f_statistik) <- 0:7
f_statistik

# kumulierte relative Häufigkeiten
F_analysis <- cumsum(f_analysis)
F_statistik <- cumsum(f_statistik)

plot(ecdf(X_analysis), verticals = TRUE,  col = "blue", 
     xlim = c(0, max(c(X_analysis, X_statistik) + 3)), main="ECDF", xlab = "kumulierte Häufigkeiten")
plot(ecdf(X_statistik),  verticals = TRUE, add = TRUE, col = "red")
legend("bottomright", legend=c("Analysis", "Statistik"),
       col=c("red", "blue"), lty = 1, cex=0.8)

quantile(X_analysis, type = 1)
quantile(X_statistik, type = 1)



# Aufgabe 5
Besucherzahl <- 41:51
Anzahl_Tage <- c(1, 9 , 13, 13, 20, 15, 10, 7, 5, 4, 3)

X <- do.call(rep, args = list(x = Besucherzahl, times = Anzahl_Tage)) # Alle 100 Beobachtungen

f_X <- Anzahl_Tage/sum(Anzahl_Tage) #relative Häufigkeiten
names(f_X) <- Besucherzahl
f_X

F_X <- cumsum(f_X) # kumulierte relative Häufigkeiten

# Absolute Häufigkeiten
barplot(f_X * 100)

# Empirische Verteilungsfunktion
plot(ecdf(X), main="ECDF", xlab = "kumulierte Häufigkeiten")


# Anteil Tage mit weniger als 270/6 = 45 Besuchern
# P(X < 45) = P(X <= 44) = F(44)
F_X["44"]


# Anteil Tage mit mindestens 45 aber maximal 50 Besuchern
# P(45 <= X <= 50) = F(50) - F(44)
F_X["50"] - F_X["44"]





