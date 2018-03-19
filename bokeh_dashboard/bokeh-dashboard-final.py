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


# Scatter Plot
scatter = figure(plot_height=350, plot_width=350, title="Scatter-Plot",
                 x_axis_label='total_bill',
                 y_axis_label='tip')

source_scatter = ColumnDataSource(data=dict(x=tips.total_bill, y=tips.tip))
# Darstellung des Scatter Plots
scatter.scatter(x='x', y='y', source=source_scatter)


# WIDGTS
# Select Histogramm
select_hist = Select(title="Histogramm", value="tip", options=list(tips.columns))

# Select Scatter
select_x = Select(title="Scatter X", value="total_bill", options=['total_bill', 'size', 'tip'])
select_y = Select(title="Scatter Y", value="tip", options=['total_bill', 'size', 'tip'])

# Callbacks zum Aktualisieren der Visualisierungen
def update_hist(data):
    """Aktualisierung der Diagramme"""
    if is_categorical_dtype(data) is True:
        summary = data.value_counts()
        hist_cat.x_range.factors = list(summary.index)
        source_hist_cat.data = dict(x=list(summary.index), top=summary.values)
        hist_cat.yaxis.axis_label = select_hist.value
    else:
        top_hist, x_hist = np.histogram(data)
        source_hist_num.data = dict(x= x_hist[:-1], top=top_hist)
        hist_num.yaxis.axis_label = select_hist.value


def update_data(attrname, old, new):
    """Update der Daten sowie der Beschriftungen"""
    # Achsenbeschriftung aktualisieren
    scatter.xaxis.axis_label = select_x.value
    scatter.yaxis.axis_label = select_y.value
    x = select_x.value
    y = select_y.value
    hist_variable = select_hist.value
    update_hist(tips[hist_variable])
    source_scatter.data = dict(x=tips[x], y= tips[y])


for w in [select_hist, select_x, select_y]:
    w.on_change('value', update_data)

# Hinzufügen des Histograms in das Hauptdokument
curdoc().add_root(row(select_hist, select_x, select_y, width=400))
curdoc().add_root(row(hist_num, scatter, width=400))
curdoc().add_root(row(hist_cat, width=400))
