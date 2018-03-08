if(!require(lubridate)) {
  install.packages("lubridate")
  require(lubridate)
}
if(!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(scales)) {
  install.packages("scales")
  require(scales)
}
if(!require(grid)) {
  install.packages("grid")
  require(grid)
}

# ———
# Daten importieren
# ———
# Daten der Fussgänger- und Velozählung (https://data.stadt-zuerich.ch/dataset/verkehrszaehlungen-werte-fussgaenger-velo)
verkehr <- read.csv("2015_verkehrszaehlungen_werte_fussgaenger_velo.csv")
# Standorte der Zählgeräte (https://data.stadt-zuerich.ch/dataset/verkehrszaehlungen-standorte-velo-fussgaenger)
standorte <- read.csv("verkehrszaehlungen_standorte_velo_fussgaenger.csv", sep = ";")
# Luftqualität (enthält Tagestemperaturen) (https://data.stadt-zuerich.ch/dataset/luftqualitaet-historisierte-messungen)
temperatur <- read.csv("ugzluftqualitaetsmessungseit2012.csv", sep = ";")

# ———
# 1. Daten aufbereiten
# ———
# 1.a Vekehrsdaten

# Datum parsen (R hat die Datums-Spalte als Text interpretiert, das Umwandeln in
# ein Datum mit Uhrzeit hilft uns nacher beim Aggregieren und filtern)
verkehr$Datum <- as.POSIXct(verkehr$Datum, "Europe/Zurich", "%Y-%m-%dT%H:%M:%S")

# Nicht alle Zählgeräte messen Velos, daher löschen wir alle Zeilen, die keinen
# Wert in der Spalte Velo_in haben.
verkehr <- verkehr[!is.na(verkehr$Velo_in),]

# Die meisten Messtationen messen in zwei Richtungen, um die Gesamtzahl zu er-
# halten, addieren wir beide Saplten miteinander. Bei Messstationen, die keinen
# Wert (NA) für Velo_out haben, erhalten wir als Gesamtwert ebenfalls NA, daher
# überschreiben wir deren Werte mit Velo_in
verkehr$Velo <- verkehr$Velo_in + verkehr$Velo_out
verkehr[is.na(verkehr$Velo),"Velo"] <- verkehr[is.na(verkehr$Velo),"Velo_in"]

# Löschen unbenötigter Tabellenspalten, das ist nicht unbedingt nötig, hilft aber
# der Übersicht
verkehr$ObjectID <- verkehr$Fuss_in <- verkehr$Fuss_out <- verkehr$Velo_in <- verkehr$Velo_out <- NULL

# ———
# 1.b Standorte

# Korrekturfaktoren anwenden (-> https://data.stadt-zuerich.ch/storage/f/verkehrszaehlungen_werte_fussgaenger_velo/15_12_15_Korrekturfaktoren_VZS_OGD_Veloverkehr.pdf)
standorte[standorte$BEZEICHNUN == "Andreasstrasse","Korrektur"] <- 1.14
standorte[standorte$BEZEICHNUN == "Militärbrücke","Korrektur"] <- 0.96
standorte[standorte$BEZEICHNUN == "Bertastrasse","Korrektur"] <- 1.25
standorte[standorte$BEZEICHNUN == "Mühlebachstrasse","Korrektur"] <- 1.3
standorte[standorte$BEZEICHNUN == "Binzmühlestrasse","Korrektur"] <- 1.2
standorte[standorte$BEZEICHNUN == "Mythenquai","Korrektur"] <- 1.2
standorte[standorte$BEZEICHNUN == "Bucheggplatz","Korrektur"] <- 1.25
standorte[standorte$BEZEICHNUN == "Saumackerstrasse","Korrektur"] <- 1.23
standorte[standorte$BEZEICHNUN == "Fischerweg","Korrektur"] <- 1.0
standorte[standorte$BEZEICHNUN == "Scheuchzerstrasse","Korrektur"] <- 1.06
standorte[standorte$BEZEICHNUN == "Hofwiesenstrasse","Korrektur"] <- 1.33
standorte[standorte$BEZEICHNUN == "Schulstrasse","Korrektur"] <- 1.37
standorte[standorte$BEZEICHNUN == "Kloster-Fahr-Weg","Korrektur"] <- 1.09
standorte[standorte$BEZEICHNUN == "Sihlpromenade","Korrektur"] <- 1.10
standorte[standorte$BEZEICHNUN == "Langstrasse (Unterführung West)","Korrektur"] <- 0.95
standorte[standorte$BEZEICHNUN == "Talstrasse","Korrektur"] <- 1.35
standorte[standorte$BEZEICHNUN == "Limmatquai, Ri. Bellevue","Korrektur"] <- 1.52
standorte[standorte$BEZEICHNUN == "Tödistrasse","Korrektur"] <- 1.37
standorte[standorte$BEZEICHNUN == "Limmatquai, Ri. Central","Korrektur"] <- 1.35
standorte[standorte$BEZEICHNUN == "Zollstrasse","Korrektur"] <- 1.43
standorte[standorte$BEZEICHNUN == "Lux-Guyer-Weg","Korrektur"] <- 1.04

# Löschen unbenötigter Tabellenspalten
standorte$ABKUERZUNG <- standorte$BIS <- standorte$RICHTUNG_I <- standorte$RICHTUNG_O <- standorte$VON <- standorte$Easting <- standorte$Northing <- NULL

# ———
# 1.c Standorte und Messwerte verschmelzen

velos <- merge(verkehr, standorte, by.x = "Standort", by.y = "FK_ZAEHLER")
# Korrekturfaktor anwenden
velos$Velo <- velos$Velo * velos$Korrektur
# Variablen standorte und verkehr löschen
rm(standorte, verkehr)

# ———
# 1.d Temperatur

# Nur Datums- und Temperaturspalte übernehmen, anschliessend Spalten umbenennen
temperatur <- data.frame(temperatur$Datum, temperatur$X.Zürich.Stampfenbachstrasse.7)
names(temperatur) <- c("Datum", "Temperatur")

# Datum parsen
temperatur$Datum <- as.POSIXct(temperatur$Datum, "Europe/Zurich", "%d.%m.%Y")
# Da die CSV-Datei mehrere Header-Zeilen hat, müssen wir diese noch löschen.
temperatur <- temperatur[!is.na(temperatur$Datum),]
# Wir brauchen ausserdem nur die 2015er Daten
temperatur <- temperatur[year(temperatur$Datum) == 2015,]
# Und die Temperatur muss noch in einen numerischen Wert umgewandelt werden
temperatur$Temperatur <- as.numeric(as.character(temperatur$Temperatur))

# ———
# 2. Wann fahren die meisten Velos in Zürich?
# ———

# Messwerte tageweise aggregieren
# (Dafür installieren wir uns das Package lubridate)

# mit Floordate können wir das Datum mit Uhrzeit auf das Datum abrunden
velos$Tag <- floor_date(velos$Datum, unit = "day")
# Jetzt summieren wir die Gesamtwerte aller Messungen des gleichen Tags.
# Dabei werden auch die unterschiedlichen Zählstationen miteinander addiert.
Tagestotal <- aggregate(Velo ~ Tag, data=velos, sum)

# Aggregierte Werte zeichnen, das geht mit dem Package ggplot2

# Zum Plotten geben wir den Datensatz (Tagestotal) und die Spalten für die X- und
# Y-Achse an. Mit geom_line() Zeichnen wir ein Liniendiagramm
ggplot(Tagestotal, aes(Tag, Velo)) +
  geom_line()

# Alternativ zum Jahresverlauf lässt sich beispielsweise auch der Tagesverlauf
# nach Wochentag aggregieren. Dafür brauchen wir den Wochentag und die Uhrzeit
# in seperaten Spalten.
velos$Wochentag <- wday(velos$Datum)
# Für die Uhrzeit setzen wir dabei einfach das Datum auf den 01.01.2015
velos$Uhrzeit <- as.POSIXct(paste0("2015-01-01 ", hour(velos$Datum), ":", minute(velos$Datum)), tz = "Europe/Zurich")
# wiederum aggregieren
Tagesvergleich <- aggregate(Velo ~ Wochentag + Uhrzeit, data=velos, mean)
# Beim Zeichnen verknüpfen wir die Spalte Wochentag mit der Linienfarbe.
ggplot(Tagesvergleich, aes(Uhrzeit, Velo, color=factor(Wochentag))) +
  geom_line()

# Jetzt müssen wir noch die Beschriftung der Legende und der X-Achse anpassen.
# Für letzteres brauchen wir das package scales

wochentage <- c("Sonntag","Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag")
ggplot(Tagesvergleich, aes(Uhrzeit, Velo, color=factor(Wochentag, labels = wochentage))) +
  geom_line() + 
  scale_x_datetime(labels=date_format("%H:%M Uhr", tz= "Europe/Zurich")) + 
  labs(color = "Wochentag")

# ———
# 3. Wo fahren die meisten Velos?
# ———
# Hierfür aggregieren wir die Messwerte nach Zählstelle:
Stellentotal <- aggregate(Velo ~ BEZEICHNUN, data=velos, mean)
# Und zeichnen (diesmal Balken)
ggplot(Stellentotal, aes(BEZEICHNUN, Velo)) +
  geom_bar(stat = "identity")

# Wieder passend wir die Achsen etwas an
ggplot(Stellentotal, aes(BEZEICHNUN, Velo)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Bezeichnung")

# Ausserdem können wir die Balken noch nach Aufkommen ordnen
ggplot(Stellentotal, aes(reorder(BEZEICHNUN, Velo), Velo)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Bezeichnung")

# ———
# 4. Wie sehr lassen sich Velofahrerinnen und -fahrer von tiefen Temperaturen vom
#    Velofahren abahlten?
# ———

# Zeichnen wir erstmal die Temperaturkurve
ggplot(temperatur, aes(Datum, Temperatur)) + geom_line()

# Das sieht ähnlich aus, wie die Kurve der Velo-Messwerte. Zum besseren Vegleich
# installieren wir das Package grid, mit dem wie mehrere Graphen nebeneinander
# zeichnen können

# Hier nutzen wir den package grid

# Anstatt die Graphen direkt zu zeichnen, werden sie Variablen zugewiesen
plot_velo <- ggplotGrob(ggplot(Tagestotal,  aes(Tag, Velo)) + geom_line())
plot_temp <- ggplotGrob(ggplot(temperatur, aes(Datum, Temperatur)) + geom_line())
# Und dann im Grid gezeichnet
grid.draw(rbind(plot_velo, plot_temp, size = "first"))

# Mit geom_smooth können wir zusätzlich eine weichere Kurve zeichen, dadurch
# treten beispielsweise die Wochenends-Wochentags-Unterschiede in den Hintergrund
# und die Graphen werden besser vergleichbar.
plot_velo <- ggplotGrob(ggplot(Tagestotal, aes(Tag, Velo)) +
                          geom_line(color="green") +
                          geom_smooth(span = .1, se= FALSE, color = "red"))

plot_temp <- ggplotGrob(ggplot(temperatur, aes(Datum, Temperatur)) +
                            geom_line(color="green") +
                            geom_smooth(span = .1, se= FALSE, color = "red"))

grid.draw(rbind(plot_velo, plot_temp, size = "first"))

# ———
# 5. Bonus: Themes
# ———

# Mit Themes lässt sich das Layout und Aussehen der Plots nach belieben anpassen.
# Mehr dazu hier: http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
t <- theme(plot.title = element_text(size=16, face="bold", colour = "#333333", margin = margin(10, 0, 10, 0), hjust = 0),
   panel.background = element_rect(fill = "#FFFFFF"),
   panel.grid.major = element_line(colour = "#EEEEEE", size = .5),
   panel.grid.minor = element_line(colour = "#EEEEEE", size = .5),
   plot.background = element_rect(fill = "#EEEEEE", colour = "#EEEEEE"),
   axis.ticks = element_line(colour = "#999999", size = .5),
   axis.text.x = element_text(colour = "#777777", margin = margin(5, 0, 3, 0)),
   axis.text.y = element_text(angle = 90, colour = "#777777", margin = margin(0, 5, 0, 3), hjust = 0.5),
   axis.title = element_text(colour = "#333333", face = "bold"),
   axis.title.y = element_text(angle = 90, margin = margin(0, 5, 0, 3), hjust = 0),
   axis.title.x = element_text(margin = margin(5, 0, 3, 0), hjust = 0),
   plot.margin = margin(0,10,5,5))

# Beim Zeichnen der Graphen kann man noch weitere Anpassungen machen:
plot_velo <- ggplotGrob(ggplot(Tagestotal, aes(Tag, Velo)) +
  geom_line(color="#DDDDDD") +
  geom_point(size=.5, color = "#CC6788") + # Verdeutlicht die Varianzen
  geom_smooth(span = .1, se= FALSE, color = "#5182B3") +
  t +
  ggtitle("Veloaufkommen im Vergleich zur Tagestemperatur, Zürich 2015") + # Titel des Graphen
  xlab("") + ylab("Anzahl Velos") + # Achsenbeschriftungen
  theme(axis.ticks.x = element_blank(),axis.text.x = element_blank(), plot.margin = margin(0,10,-13,5)) + # Theme Anpassen um die X-Achsenbeschriftung beim ersten Graphen zuu entfernen
  scale_y_continuous(expand = c(0, 0), limits = c(0,55000), labels = function (x, ...) format(x, ..., big.mark = " ", scientific = FALSE, trim = TRUE)) + # Leerzeichen für bessere Lesbarkeit einfügen
  scale_x_datetime(date_breaks="1 month")) # Gleiches Grid, wie im zweiten Graphen zeichnen



plot_temp <- ggplotGrob(ggplot(temperatur, aes(Datum, Temperatur)) +
  geom_line(color="#DDDDDD") +
  geom_point(size=.5, color = "#CC6788") +
  geom_smooth(span = .1, se= FALSE, color = "#5182B3") +
  t +
  scale_y_continuous(expand = c(0, 0), limits = c(-5,35)) +
  scale_x_datetime(labels=date_format("%b", tz= "Europe/Zurich"), date_breaks="1 month") + # Monats-Label ohne Jahr und für jeden Monat zeichnen
  xlab(""))

grid.draw(rbind(plot_velo, plot_temp, size = "first"))

# Fertig \o/
# Mehr zu R gibt's hier:
# http://journocode.com/
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
# http://srfdata.github.io/
# http://rddj.info/
# http://stackoverflow.com/questions/tagged/r