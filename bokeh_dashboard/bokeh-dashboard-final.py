import numpy as np
from seaborn import load_dataset
from bokeh.io import curdoc
from bokeh.layouts import row
from bokeh.models import ColumnDataSource
from bokeh.plotting import figure
from bokeh.models.widgets import Select

# Festlegen des Dashboard Titles
curdoc().title = "Tips Dashboard"

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
                 x_axis_label='total_bill',
                 y_axis_label='tip')

source_scatter = ColumnDataSource(data=dict(x=tips.total_bill, y=tips.tip))

# Darstellung des Scatter Diagramms
scatter.scatter(x='x', y='y', source=source_scatter)

# WIDGTS
# Select Histogramm
select_hist = Select(title="Histogramm", value="total_bill",
                     options=list(tips.dtypes[tips.dtypes != 'category'].index))

select_cat = Select(title="Säulendiagramm", value="smoker",
                    options=list(tips.dtypes[tips.dtypes == 'category'].index))

# Select Scatter
select_x = Select(title="Scatter X", value="total_bill", options=['total_bill', 'size', 'tip'])
select_y = Select(title="Scatter Y", value="tip", options=['total_bill', 'size', 'tip'])

# Callbacks zum Aktualisieren der Visualisierungen
def update_data(attrname, old, new):
    """Update der Daten sowie der Beschriftungen"""
    # Scatter Diagramm
    scatter.xaxis.axis_label = select_x.value
    scatter.yaxis.axis_label = select_y.value
    x = select_x.value
    y = select_y.value
    source_scatter.data = dict(x=tips[x], y= tips[y])
    # Säulendiagramm
    data_cat = tips[select_cat.value]
    summary = data_cat.value_counts()
    bar.x_range.factors = list(summary.index)
    source_kat.data = dict(x=list(summary.index), top=summary.values)
    bar.xaxis.axis_label = select_cat.value
    # Historamm
    data_hist = tips[select_hist.value]
    top_hist_new, x_hist_new = np.histogram(data_hist)
    source_hist.data = dict(x= x_hist_new[:-1], top=top_hist_new)
    hist.xaxis.axis_label = select_hist.value

for w in [select_hist, select_cat, select_x, select_y]:
    w.on_change('value', update_data)

# Hinzufügen der Diagramme und Select Buttons in das Hauptdokument
curdoc().add_root(row(select_hist, select_cat, select_x, select_y))
curdoc().add_root(row(hist, bar, scatter))
