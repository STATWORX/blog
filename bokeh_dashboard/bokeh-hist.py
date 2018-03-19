import numpy as np
from seaborn import load_dataset
from bokeh.io import curdoc
from bokeh.layouts import row
from bokeh.models import ColumnDataSource
from bokeh.plotting import figure
from bokeh.models.widgets import Select
from pandas.api.types import is_categorical_dtype

# Festlegen des Dashboard Titles
curdoc().title = "Histogramm und Scatter Plot"

# Datenset laden
tips = load_dataset("tips")

# VISUALISIERUNGEN
# Histogramm mit Numpy erstellen
top_hist_num, x_hist_num = np.histogram(tips.total_bill)

# Daten in Dict überführen
source_hist_num = ColumnDataSource(data=dict(x=x_hist_num[:-1], top=top_hist_num))

# Histogramm mit Verhältnisskala
hist_num = figure(plot_height=350, plot_width=350, title="Histogramm - Verhältnisskala",
                  y_axis_label='total_bill')

# Darstellung des Histograms
hist_num.vbar(x='x', top='top', width=0.5, source=source_hist_num)

# Kategoriale Variablen
hist_cat_data = tips.smoker.value_counts()
x_hist_cat = list(hist_cat_data.index)
top_hist_cat = hist_cat_data.values

# Daten in Dict überführen
source_hist_cat = ColumnDataSource(data=dict(x=x_hist_cat, top=top_hist_cat))

# Histogramm mit Nominalskala
hist_cat = figure(x_range= x_hist_cat, plot_height=350, plot_width=350, title="Histogramm - Nominalskala",
                  y_axis_label='smoker')

# Darstellung des Histograms
hist_cat.vbar(x='x', top='top', width=0.5, source=source_hist_cat)

# Hinzufügen des Histograms in das Hauptdokument
curdoc().add_root(row(hist_num, hist_cat, width=800))
