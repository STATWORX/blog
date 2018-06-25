from selenium import webdriver
import pandas as pd
from time import sleep

# Setting up the Chromedriver
driver = webdriver.Chrome(executable_path="Path to Chromedriver")

# Call Website
driver.get("http://fussballmathe.de/simulation/")

# dict for translating the country names
names = {
 "Russland"     : "Russia" ,
 "Saudi-Arabien": "Saudi Arabia",
 "Uruguay"      : "Uruguay",
 "Ägypten"      : "Egypt",
 "Portugal"     : "Portugal",
 "Marokko"      : "Morocco",
 "Iran"         : "Iran",
 "Spanien"      : "Spain",
 "Frankreich"   : "France",
 "Australien"   : "Australia",
 "Peru"         : "Peru",
 "Dänemark"     : "Denmark",
 "Argentinien"  : "Argentina",
 "Island"       : "Island",
 "Kroatien"     : "Croatia",
 "Nigeria"      : "Nigeria",
 "Brasilien"    : "Brazil",
 "Schweiz"      : "Switzerland",
 "Costa Rica"   : "Costa Rica",
 "Serbien"      : "Serbia",
 "Deutschland"  : "Germany",
 "Mexico"       : "Mexico",
 "Schweden"     : "Sweden",
 "Südkorea"     : "South Korea",
 "Belgien"      : "Belgium",
 "Panama"       : "Panama",
 "Tunesien"     : "Tunesia",
 "England"      : "England",
 "Polen"        : "Poland",
 "Senegal"      : "Senegal",
 "Kolumbien"    : "Colombia",
 "Japan"        : "Japan"}

# Aligning games to groups
games = [1,17,-34,33,-19,2,
  4,18,-35,36,-20,3,
  5,22,-38,37,-21,7,
  6,23,-39,40,-25,8,
  11,24,-43,44,-26,9,
  10,29,-42,41,-28,12,
  13,27,-45,47,-30,14,
  16,32,-48,46,-31,15]


# Starting Simulations
n_simulations = 100
df_total_sim = pd.DataFrame()

for sim in range(n_simulations):
    # Find Simulation Elements
    driver.find_element_by_id('reps_save').click()
    # Getting the website hmtl code
    website = driver.page_source
    # Reading DataFrames from Website
    tables = pd.read_html(website)
    df_total = pd.DataFrame()
    for i in list(range(8,16)):
        tables[i][3] = [sim] * 6
        # Replacing country names
        for index, value in enumerate(tables[i][0]):
            if value in names:
                tables[i].iloc[index, 0] = names[value]
        for index, value in enumerate(tables[i][2]):
            if value in names:
                tables[i].iloc[index, 2] = names[value]

        df_total = df_total.append(tables[i])
    df_total[4] = games
    df_total_sim = df_total_sim.append(df_total)
    sleep(5)

# Cleaning results
df_total_sim[1] = df_total_sim[1].str.replace(':','-')
# Saving results
df_total_sim.to_csv('path to save csv')
