import os
import re
import pandas as pd
from numpy import nan
import plotnine as gg

# Config
PATH = os.getcwd()
path_n = re.split(pattern=r"/|\\", string=PATH)[1:]
if os.name == "posix":
    path_n = "/" + os.path.join(*path_n)
else:
    drive = PATH[0:3]
    path_n = drive + os.path.join(*path_n)


def clean_scenarios(row) -> pd.Series:
    """Helper function to extract select scenario details for final comparison plots."""
    row = str(row)
    sel_col = re.search(pattern="^s.*([13])", string=row).group(1) if re.match(pattern="^s.*([13])", string=row) else nan
    pos_col = re.search(pattern="^s.*[13](.*)$", string=row).group(1) if re.match(pattern="^s.*[13](.*)$", string=row) else nan
    return pd.Series([sel_col, pos_col])


# Data import and cleaning
pandas_sel = pd.read_csv(os.path.join(path_n, "output", "select_results_pandas.csv"))
pandas_filter = pd.read_csv(os.path.join(path_n, "output", "filter_results_pandas.csv"))
pandas_arr = pd.read_csv(os.path.join(path_n, "output", "arrange_results_pandas.csv"))
pandas_mut = pd.read_csv(os.path.join(path_n, "output", "mutate_results_pandas.csv"))
pandas_sum = pd.read_csv(os.path.join(path_n, "output", "summarise_results_pandas.csv"))
datatable_sel = pd.read_csv(os.path.join(path_n, "output", "select_results_datatable.csv"))
datatable_filter = pd.read_csv(os.path.join(path_n, "output", "filter_results_datatable.csv"))
datatable_arr = pd.read_csv(os.path.join(path_n, "output", "arrange_results_datatable.csv"))
datatable_mut = pd.read_csv(os.path.join(path_n, "output", "mutate_results_datatable.csv"))
datatable_sum = pd.read_csv(os.path.join(path_n, "output", "summarise_results_datatable.csv"))

renames = {"n_col": "no_column", "n_row": "data_length",
           "min_time": "min", "mean_time_ms": "avg",
           "median_time_ms": "q50", "max_time": "max"}

# format R outputs
for df in [datatable_sel, datatable_filter, datatable_arr, datatable_mut, datatable_sum]:
    df.rename(index=str, columns=renames, inplace=True)
    df[["min", "max", "avg", "q50"]] = df[["min", "max", "avg", "q50"]].apply(pd.to_numeric, downcast="float").apply(round, ndigits=2)
    df["data_length"] = df["data_length"].apply(pd.to_numeric, downcast="integer")


datatable_sel[["sel_col", "pos_col"]] = datatable_sel["scenario"].apply(clean_scenarios)
pandas_sum["scenario"] = pandas_sum["scenario"].apply(lambda x: re.sub(pattern="zipois", repl="zip", string=x))

# Wrangle results
pairs = [[pandas_sel, datatable_sel, "select"],
         [pandas_filter, datatable_filter, "filter"],
         [pandas_arr, datatable_arr, "arrange"],
         [pandas_mut, datatable_mut, "mutate"],
         [pandas_sum, datatable_sum, "summarise"]]
comparison = pd.DataFrame({"operation": [], "scenario": [], "no_column": [], "data_length": [],
                           "min": [], "max": [], "avg": [], "q50": [], "sel_col": [], "pos_col": []})

for pan, dt, op in pairs:
    no = pan.shape[0]
    pan, dt = pan.sort_values(by=["no_column", "data_length"]), dt.sort_values(by=["no_column", "data_length"])
    pan, dt = pan.reset_index(), dt.reset_index()
    comparison = comparison.append(pd.DataFrame({"operation": [op] * no,
                                                 "scenario": pan.scenario,
                                                 "no_column": pan.no_column,
                                                 "data_length": pan.data_length,
                                                 "min": round(pan["min"] / dt["min"], 1),
                                                 "max": round(pan["max"] / dt["max"], 1),
                                                 "avg": round(pan["avg"] / dt["avg"], 1),
                                                 "q50": round(pan["q50"] / dt["q50"], 1)}))

comparison[["sel_col", "pos_col"]] = comparison["scenario"].apply(clean_scenarios)
comparison[["no_column", "data_length"]] = comparison[["no_column", "data_length"]].apply(pd.to_numeric, downcast="integer")

### Visual Exploration
saveformat = "png"
## Select

# pandas
plot = (gg.ggplot(pandas_sel, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=9) +
        gg.labs(y="# Rows", x="# Columns", title="Pandas median selection time") +
        gg.facet_grid("pos_col ~ sel_col") +
        gg.theme_bw() +
        gg.theme(legend_position=None))

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"select_results_pandas.{saveformat}"), width=15, height=10)

# data.table
plot = (gg.ggplot(datatable_sel, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=9) +
        gg.labs(y="# Rows", x="# Columns", title="data.table median selection time") +
        gg.facet_grid("pos_col ~ sel_col") +
        gg.theme_bw() +
        gg.theme(legend_position=None))

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"select_results_datatable.{saveformat}"), width=15, height=10)

# comparison
plot = (gg.ggplot(comparison[comparison.operation == "select"], gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=9) +
        gg.ggtitle(title="pandas / data.table Relative median Select timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_grid("pos_col ~ sel_col") +
        gg.theme_bw()
        )

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"comparison_select.{saveformat}"), width=15, height=10)

## Filter

# pandas
plot = (gg.ggplot(pandas_filter, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title="pandas filter timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"filter_results_pandas.{saveformat}"), width=15, height=10)

# data.table
plot = (gg.ggplot(datatable_filter, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title="data.table filter timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"filter_results_datatable.{saveformat}"), width=15, height=10)

# comparison
plot = (gg.ggplot(comparison[comparison.operation == "filter"], gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title="pandas / data.table Relative median Filter timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"comparison_filter.{saveformat}"), width=15, height=10)

## arrange
operation = "arrange"

# pandas
plot = (gg.ggplot(pandas_arr, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"pandas {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"{operation}_results_pandas.{saveformat}"), width=15, height=10)

# data.table
plot = (gg.ggplot(datatable_arr, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"data.table {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"{operation}_results_datatable.{saveformat}"), width=15, height=10)

# comparison
plot = (gg.ggplot(comparison[comparison.operation == f"{operation}"], gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"pandas / data.table Relative median {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"comparison_{operation}.{saveformat}"), width=15, height=10)

## mutate
operation = "mutate"

# pandas
plot = (gg.ggplot(pandas_mut, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"pandas {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"{operation}_results_pandas.{saveformat}"), width=15, height=10)

# data.table
plot = (gg.ggplot(datatable_mut, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"data.table {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"{operation}_results_datatable.{saveformat}"), width=15, height=10)

# comparison
plot = (gg.ggplot(comparison[comparison.operation == f"{operation}"], gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"pandas / data.table Relative median {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"comparison_{operation}.{saveformat}"), width=15, height=10)

## summarise
operation = "summarise"

# pandas
plot = (gg.ggplot(pandas_sum, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"pandas {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"{operation}_results_pandas.{saveformat}"), width=15, height=10)

# data.table
plot = (gg.ggplot(datatable_sum, gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"data.table {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"{operation}_results_datatable.{saveformat}"), width=15, height=10)

# comparison
plot = (gg.ggplot(comparison[comparison.operation == f"{operation}"], gg.aes("factor(no_column)", "factor(data_length)")) +
        gg.geom_tile(gg.aes(fill="q50")) +
        gg.geom_text(gg.aes(label="q50"), color="white", size=8) +
        gg.ggtitle(title=f"pandas / data.table Relative median {operation} timings") +
        gg.labs(x="# Columns", y="# Rows") +
        gg.facet_wrap("~scenario") +
        gg.theme_bw())

gg.ggsave(plot, filename=os.path.join(path_n, "output", f"comparison_{operation}.{saveformat}"), width=15, height=10)