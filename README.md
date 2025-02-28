# medacoPlot

[![r-universe](https://joergsesterhenn.r-universe.dev/badges/medacoPlot)](https://joergsesterhenn.r-universe.dev/medacoPlot)
[![pkgdown docs](https://github.com/joergsesterhenn/medaco-plot/actions/workflows/pkgdown.yaml/badge.svg)](https://joergsesterhenn.github.io/medaco-plot/)
[![codecov](https://codecov.io/github/joergsesterhenn/medaco-plot/graph/badge.svg?token=YGPULT58WZ)](https://codecov.io/github/joergsesterhenn/medaco-plot)
[![OSV-Scanner](https://github.com/joergsesterhenn/medaco-plot/actions/workflows/osv-scanner.yml/badge.svg)](https://github.com/joergsesterhenn/medaco-plot/actions/workflows/osv-scanner.yml)
[![lintr](https://github.com/joergsesterhenn/medaco-plot/actions/workflows/lintr.yml/badge.svg)](https://github.com/joergsesterhenn/medaco-plot/actions/workflows/lintr.yml)
[![R-CMD-check](https://github.com/joergsesterhenn/medaco-plot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joergsesterhenn/medaco-plot/actions/workflows/R-CMD-check.yaml)

Moderne Smartmeter stellen Einspeise- und Lieferdaten über Onlineportale bereit.
Zusätzlich gibt es die Rohdaten auch in CSV-Form.
Die Shiny app medacoPlot erlaubt es diese in einem Ordner abgelegten CSV Dateien zu visualisieren.

# R package 

Das R package medacoPlot exportiert die Methoden zum Einlesen der Daten, zur Datenmanipulation und zum Plotten der Daten. Durch installieren des package können die Funktionen ohne die Shiny App genutzt werden.

```
install.packages(
    'medacoPlot', 
    repos = c(
        'https://joergsesterhenn.r-universe.dev', 
        'https://cloud.r-project.org'
    )
)
```

# Docker image bauen
```
docker build -t medaco-plot .
```

# Docker image ausführen
Das Verzeichnis mit den csv-Dateien muss in den Container gemounted werden. Per Default erwartet der Container die Daten im Verzeichnis **/data**. Dieser Ordner kann durch setzen der Umgebungsvariablen MEDACO_DATA überschrieben werden. Es wird rekursiv in diesem Verzeichnis nach csv Daten gesucht. Alle Daten werden aggregiert und visualisiert.
```
docker run -v /local/path/to/csvs:/data -it --rm -p 3838:3838 medaco-plot
```

Der aktuelle Stand des Images kann auch direkt aus der github container registry bezogen werden:
```
docker run -v /local/path/to/csvs:/data -it --rm -p 3838:3838 ghcr.io/joergsesterhenn/medaco-plot:main
```

# Shiny App

Das Projekt kann auch ausgecheckt und als Shiny app gestartet werden. 
Wenn die Umgebungsvariable MEDACO_DATA auf das Verzeichnis mit den zu visualisierenden Daten gesetzt ist, werden diese visualisiert. Andernfalls kann über die Oberfläche ein Ordner ausgewählt werden.

# Entwicklung

Für die Entwicklung wird R (>=4.4.2) verwendet.
Medaco-Plot wird in VSCode entwickelt.
Im Code werden Referenzen außerhalb von *base* immer explizit angegeben.

Folgende Befehle sind bei der Entwicklung hilfreich:
```
# Abhängigkeiten installieren
renv::restore() 

# Geänderte Abhängigkeiten festschreiben
renv::snapshot()

# run tests
devtools::test() 

# check coverage
covr::package_coverage()

# check package consistency
devtools::check()
```
# Vorschau

![Screenshot](https://github.com/joergsesterhenn/medaco-plot/blob/main/images/plot.png?raw=true)