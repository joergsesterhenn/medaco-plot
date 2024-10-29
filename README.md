# medacoPlot

[![codecov](https://codecov.io/github/joergsesterhenn/medaco-plot/graph/badge.svg?token=YGPULT58WZ)](https://codecov.io/github/joergsesterhenn/medaco-plot)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

Moderne Smartmeter stellen Einspeise- und Lieferdaten über Onlineportale bereit.
Zusätzlich gibt es die Rohdaten auch in CSV-Form.
Die App medacoPlot erlaubt es diese in einem Ordner abgelegten CSV Dateien zu visualisieren.

# Image bauen
```
docker build -t medaco-plot .
```

# Image ausführen

Das Verzeichnis mit den csv-Dateien muss in den Container gemounted werden. 
```
docker run -v /local/path/to/csvs:/data -it --rm -p 3838:3838 medaco-plot
```

Der aktuelle Stand des Images kann auch direkt aus der github container registry bezogen werden:
```
docker run -v /local/path/to/csvs:/data -it --rm -p 3838:3838 ghcr.io/joergsesterhenn/medaco-plot:main
```

# Vorschau

![Screenshot](https://github.com/joergsesterhenn/medaco-plot/blob/main/images/plot.png?raw=true)