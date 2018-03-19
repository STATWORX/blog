import numpy as np
from seaborn import load_dataset
from bokeh.io import curdoc
from bokeh.layouts import row
from bokeh.models import ColumnDataSource
from bokeh.plotting import figure

# Datenset laden
tips = load_dataset("tips")
# Histogramm mit Numpy erstellen
top_hist, x_hist = np.histogram(tips.total_bill)
# Set up data
source_hist = ColumnDataSource(data=dict(x=x_hist[:-1], top=top_hist))
# Set up plot
plot = figure(plot_height=400, plot_width=400, title="Histogramm",
              tools="crosshair,pan,reset,save,wheel_zoom")

plot.vbar(x='x', top='top', width=2, source=source_hist)

curdoc().add_root(row(plot, width=800))
curdoc().title = "Histogramm"
