# Medaco Plot

Moderne Smartmeter stellen Einspeise- und Lieferdaten meist über Onlineportale bereit.
Zusätzlich gibt es diese auch als Rohdaten auch in CSV-Form.

Diese App erlaubt es die in einem Ordner abgelegten Dateien zu visualisieren.

# Build

docker build -t medaco-plot .


# Run

docker run -v /local/path/to/csvs:/data -it --rm -p 3838:3838 medaco-plot
