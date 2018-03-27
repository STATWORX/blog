import os
import re
import json
import time
import numpy as np
import pandas as pd

# Config
PATH = os.getcwd()
path_n = re.split(pattern=r"/|\\", string=PATH)[1:]
if os.name == "posix":
    path_n = "/" + os.path.join(*path_n)
else:
    drive = PATH[0:3]
    path_n = drive + os.path.join(*path_n)

RUNS = 100


def infer_column_cats(dir: "Path to working directoty.") -> tuple:
    """Helper function to identify dataset sizes based on file names."""
    files = os.listdir(os.path.join(dir, "data"))
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


scenarios = json.load(open(os.path.join(path_n, "output", "summarise.JSON")))
nrows, ncols = infer_column_cats(path_n)
timings, results = [], []

for col in ncols:
    print(f"-Column: {col}--")
    for row in nrows:
        print(f"--Row: {row}")
        data = pd.read_csv(os.path.join(path_n, "data", f"sim_data_{col}_{row}.csv"))

        # without group by
        for i, scenario in enumerate(scenarios[col]["summarise"]):
            print(f"---Scenario {i+1}: {scenario}---")
            sel = re.search(pattern=r'([A-Z]{3})', string=scenario).group(1)
            print(sel)
            if sel in ["INT", "STR", "LGL"]:
                func = f"temp['{scenario}'].value_counts()"
            elif sel == "DBL":
                func = f"temp['{scenario}'].describe()"
            else:
                raise ValueError

            for j in range(RUNS):
                temp = data
                timings.append(time_function(func=func))
                temp = None
            results.append(create_stats(measures=timings, col=col, row=row, scenario=f"summarise_{sel}"))
            print("Without GroupBy")
            print(results[-1])
            timings = []

            # with group by
            for gcol in scenarios[col]["group_by"]:
                if sel in ["INT", "STR", "LGL"]:
                    ops = {f'{scenario}': 'count'}
                    func = f"temp.groupby('{gcol}').aggregate({ops})"
                elif sel == "DBL":
                    ops = {f'{scenario}': ['mean', 'median', 'count']}
                    func = f"temp.groupby('{gcol}').aggregate({ops})"
                else:
                    raise ValueError

                grouper = re.search(pattern=r"[A-Z]{3}_[0-9]+_([a-z]{3,6})_", string=gcol).group(1)

                for j in range(RUNS):
                    temp = data
                    timings.append(time_function(func=func))
                    temp = None
                results.append(create_stats(measures=timings, col=col, row=row, scenario=f"summarise_{sel}_by_{grouper}"))
                print("With GroupBy")
                print(results[-1])
                timings = []


results_df = pd.DataFrame(results)
results_df[["data_length", "no_column"]] = results_df[["data_length", "no_column"]].apply(pd.to_numeric,
                                                                                          axis=1,
                                                                                          downcast="integer")
results_df.sort_values(["data_length", "no_column"])
results_df[["min", "max", "q50", "avg"]] = round(results_df[["min", "max", "q50", "avg"]] * 1000, 2)
# results_df["sel_col"] = results_df["scenario"].apply(lambda x: re.search(pattern="([13])", string=x).group(1))
# results_df["pos_col"] = results_df["scenario"].apply(lambda x: re.search(pattern="[13](.*)$", string=x).group(1))
results_df.to_csv(os.path.join(path_n, "output", "summarise_results_pandas.csv"), index=False)