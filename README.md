# Medaco Plot

[![codecov](https://codecov.io/github/joergsesterhenn/medaco-plot/graph/badge.svg?token=YGPULT58WZ)](https://codecov.io/github/joergsesterhenn/medaco-plot)

Moderne Smartmeter stellen Einspeise- und Lieferdaten meist über Onlineportale bereit.
Zusätzlich gibt es diese auch als Rohdaten auch in CSV-Form.

Diese App erlaubt es die in einem Ordner abgelegten CSV Dateien zu visualisieren.

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