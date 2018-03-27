import os
import re
import json
import time
import numpy as np
import pandas as pd
from plotnine import *

# Config
PATH = os.getcwd()
path_n = re.split(pattern=r"/|\\", string=PATH)[1:]
if os.name == "posix":
    path_n = "/" + os.path.join(*path_n)
else:
    drive = PATH[0:3]
    path_n = drive + os.path.join(*path_n)
RUNS = 100


def infer_column_cats(path: "Path to working directoty.") -> tuple:
    """Helper function to identify dataset sizes based on file names."""
    files = os.listdir(os.path.join(path, "data"))
    cats = set([re.match(pattern=".*_(.*).csv$", string=file).group(1) for file in files])
    cols = set([re.match(pattern=".*_(.*)_.*.csv$", string=file).group(1) for file in files])
    return cats, cols


def time_function(func: "Function call to be evaluted as str.") -> float:
    """Helper function to time data access."""
    start = time.time()
    eval(func)
    return time.time() - start


def create_stats(measures: "List of function timings.",
                 col: "Current Column.", row: "Current Row",
                 scenario: "Current Scenario.") -> dict:
    """Helper function to create result dataset."""
    return {"scenario": scenario,
            "no_column": col,
            "data_length": row,
            "min": np.min(measures),
            "max": np.max(measures),
            "avg": np.mean(measures),
            "q50": np.median(measures)}


scenarios = json.load(open(os.path.join(path_n, "output", "filter.JSON")))
nrows, ncols = infer_column_cats(path_n)
timings, results = [], []

for col in ncols:
    print(f"-Column: {col}--")
    for row in nrows:
        print(f"--Row: {row}")
        data = pd.read_csv(os.path.join(path_n, "data", f"sim_data_{col}_{row}.csv"))
        sel = [(target_col, scenarios[col][target_col][-1]) for target_col in scenarios[col]]
        print(f"Scenario: {scenarios[col]} Selection: {sel}")
        funcs = [f"temp[temp['{sel[0][0]}'] >= {sel[0][1]}]",
                 f"temp[temp['{sel[1][0]}'] >= {sel[1][1]}]",
                 f"temp[temp['{sel[2][0]}'] == '{sel[2][1]}']",
                 f"temp[temp['{sel[3][0]}'] == {sel[3][1]}]",
                 f"""temp[(temp['{sel[0][0]}'] >= {sel[0][1]}) &
                         (temp['{sel[1][0]}'] >= {sel[1][1]})]""",
                 f"""temp[(temp['{sel[0][0]}'] >= {sel[0][1]}) &
                         (temp['{sel[1][0]}'] >= {sel[1][1]}) &
                         (temp['{sel[2][0]}'] == '{sel[2][1]}')]""",
                 f"""temp[(temp['{sel[0][0]}'] >= {sel[0][1]}) &
                         (temp['{sel[1][0]}'] >= {sel[1][1]}) &
                         (temp['{sel[2][0]}'] == '{sel[2][1]}') &
                         (temp['{sel[3][0]}'] == {sel[3][1]})]"""]
        for i, fun in enumerate(funcs):
            print(i, fun)
            for j in range(RUNS):
                temp = data
                timings.append(time_function(func=fun))
                temp = None

            results.append(create_stats(col=col, row=row, measures=timings, scenario=i + 1))
            print(results[-1])
            timings = []

result_df = pd.DataFrame(results)
result_df[["data_length", "no_column"]] = result_df[["data_length", "no_column"]].apply(pd.to_numeric,
                                                                                        axis=1,
                                                                                        downcast="integer")
result_df[["min", "max", "q50", "avg"]] = round(result_df[["min", "max", "q50", "avg"]] * 1000, 2)
result_df.sort_values(["data_length", "no_column"], inplace=True)
result_df.to_csv(os.path.join(path_n, "output", "filter_results_pandas.csv"), index=False)