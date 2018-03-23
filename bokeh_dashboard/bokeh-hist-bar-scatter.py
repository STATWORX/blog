import numpy as np
from seaborn import load_dataset
from bokeh.io import curdoc
from bokeh.layouts import row
from bokeh.models import ColumnDataSource
from bokeh.plotting import figure

# Festlegen des Dashboard Titles
curdoc().title = "Histogramm/Säulendiagramm und Scatter Plot"

# Datenset laden
tips = load_dataset("tips")

# VISUALISIERUNGEN
# Histogramm mit Numpy erstellen
top_hist, x_hist = np.histogram(tips.total_bill)

# Daten in Dict überführen
source_hist = ColumnDataSource(data=dict(x=x_hist[:-1], top=top_hist))

# Allgemeinen Plot erstellen
hist = figure(plot_height=400, plot_width=400, title="Histogramm",
              x_axis_label='total_bill', y_axis_label='Absolute Häufigkeit')

# Darstellung des Säulendiagramms
hist.vbar(x='x', top='top', width=0.5, source=source_hist)

# Kategoriale Variablen
kat_data = tips.smoker.value_counts()
x_kat = list(kat_data.index)
top_kat = kat_data.values

# Daten in Dict überführen
source_kat = ColumnDataSource(data=dict(x=x_kat, top=top_kat))

# Allgemeinen Plot erstellen
bar = figure(x_range= x_kat, plot_height=400, plot_width=400, title="Säulendiagramm",
             x_axis_label='smoker', y_axis_label='Absolute Häufigkeit')

# Darstellung des Säulendiagramm
bar.vbar(x='x', top='top', width=0.1, source=source_kat)

# Allgemeinen Plot erstellen
scatter = figure(plot_height=400, plot_width=400, title="Scatter-Plot",
                 tools="crosshair,pan,reset,save,wheel_zoom",
                 x_axis_label='total_bill',
                 y_axis_label='tip')

source_scatter = ColumnDataSource(data=dict(x=tips.total_bill, y=tips.tip))
# Darstellung des Scatter Diagramms
scatter.scatter(x='x', y='y', source=source_scatter)

# Hinzufügen des Scatter Plots in das Hauptdokument
curdoc().add_root(row(hist, bar, scatter))
