# %%
# installation guide:
# # plotly: please refer to the official plotly.py repository
# https://github.com/plotly/plotly.py#installation
# # numpy: you can install it via the terminal using
# pip install numpy


import plotly.graph_objs as go
import numpy as np


x_values = np.random.randn(5000)
figure = go.FigureWidget(data=[go.Histogram(x=x_values, nbinsx=10)], layout=go.Layout(xaxis={'range': [-4, 4]}, bargap=0.05))
histogram = figure.data[0]

def adjust_histogram_data(xaxis, xrange):
    x_values_subset = x_values[np.logical_and(xrange[0] <= x_values, x_values <= xrange[1])]
    histogram.x = x_values_subset
figure.layout.xaxis.on_change(adjust_histogram_data, 'range')


display(figure)  # show the figure in the cell output

# %%
# default plotly histogram, so that you can compare the output
default_histogram = go.FigureWidget([go.Histogram(x=x_values, nbinsx=10)], go.Layout(xaxis={'range': [-4, 4]}, bargap=0.05))
display(default_histogram)


# %%

