
#Import NRC event report file with | delimiter. (though you can change the delimiter in the code if you wish)
#The file will be split into different 'Event Descriptions'.
#Each 'Event Descriptions' section will be saved in a file named by the user, 
#with the date and .csv file extension added.
#The purpose of splitting the NRC file by 'Event Description' is that each has
#differing questions which the user may wish to pose, due to different variables. 






from datetime import datetime
from tkinter import messagebox, filedialog
import pandas as pd
import numpy as np

import tkinter as tk
from tkinter import filedialog, messagebox

root = tk.Tk()

file = filedialog.askopenfilenames()

file = str(file)

stripped_file = file.strip("('',)")

nrc_event_file = pd.read_csv(stripped_file, delimiter = '|')

nrc_event_file_sorted = nrc_event_file.sort_values(by=['Event Desc'])

nrc_event_file_sorted = nrc_event_file_sorted.reset_index(level=None, drop=True, inplace=False, col_level=0, col_fill='')

df = pd.DataFrame(nrc_event_file_sorted)

total_row_count = 0
for i, row in df.iterrows():
    total_row_count += 1

k = total_row_count
AS_row_count = 0
PR_row_count = 0
FCF_row_count = 0
P21_row_count = 0
total_row_range = range(0, k)

#-----------------------------------#

for i in total_row_range:
    if df.iloc[i,0] == "Agreement State":
        AS_row_count += 1

for i in total_row_range:
    if df.iloc[i,0] == "Power Reactor":
        PR_row_count += 1 

for i in total_row_range:
    if df.iloc[i,0] == "Fuel Cycle Facility":
        FCF_row_count += 1 

for i in total_row_range:
    if df.iloc[i,0] == "Part 21":
        P21_row_count += 1
 
AS_rows, df_cols = (AS_row_count, len(df.columns))
AS_array = [[0]*df_cols]*AS_rows

PR_rows, df_cols = (PR_row_count, len(df.columns))
PR_array = [[0]*df_cols]*PR_rows

FCF_rows, df_cols = (FCF_row_count, len(df.columns))
FCF_array = [[0]*df_cols]*FCF_rows

P21_rows, df_cols = (P21_row_count, len(df.columns))
P21_array = [[0]*df_cols]*P21_rows

df_AgState = pd.DataFrame(AS_array)
df_PowReac = pd.DataFrame(PR_array)
df_Fuel_Cyc_Fac = pd.DataFrame(FCF_array)
df_Part_21 = pd.DataFrame(P21_array)


#-----------------------------------# #-----------------------------------#

df_AgState.columns = df.columns
df_PowReac.columns = df.columns
df_Fuel_Cyc_Fac.columns = df.columns
df_Part_21.columns = df.columns

messagebox.showinfo("showinfo", "The date and .csv extension will automatically be added to the file name you provide.")

i = 0
for l in range(0, AS_row_count):
    if df.iloc[l,0] == "Agreement State":
        df_AgState.iloc[i] = df.iloc[l]
        i += 1

df_AgState_mod = df_AgState.iloc[:, np.r_[0:7, 8, 15:26, 32:38, 70]]


date = datetime.now().strftime('%Y%m%d')

file_path = filedialog.asksaveasfilename(title = "Agreement State Report")
file_path = file_path + date + "_.csv"
df_AgState_mod.to_csv(file_path, index=False)

#-----------------------------------# #-----------------------------------#

i = 0
for l in total_row_range:
    if df.iloc[l,0] == "Power Reactor":
        df_PowReac.iloc[i] = df.iloc[l]
        i += 1

df_PowReac_mod = df_PowReac.iloc[:, np.r_[0:8, 10:12, 15:28, 32:34, 52:58, 59:63, 70]]

file_path = filedialog.asksaveasfilename(title = "Power Reactor Report")
file_path = file_path + date + "_.csv"
df_PowReac_mod.to_csv(file_path, index=False)


#-----------------------------------# #-----------------------------------#

i = 0
for l in total_row_range:
    if df.iloc[l,0] == "Fuel Cycle Facility":
        df_Fuel_Cyc_Fac.iloc[i] = df.iloc[l]
        i += 1


df_Fuel_Cyc_Fac_mod = df_Fuel_Cyc_Fac.iloc[:, np.r_[0:11, 15:25, 34:34, 70]]

file_path = filedialog.asksaveasfilename(title = "Fuel Cycle Facility Report")
file_path = file_path + date + "_.csv"
df_Fuel_Cyc_Fac_mod.to_csv(file_path, index=False)

#-----------------------------------# #-----------------------------------#

i = 0
for l in total_row_range:
    if df.iloc[l,0] == "Part 21":
        df_Part_21.iloc[i] = df.iloc[l]
        i += 1


df_Part_21_mod = df_Part_21.iloc[:, np.r_[0:8, 15:25, 34:34, 70]]

file_path = filedialog.asksaveasfilename(title = "Part 21 Report")
file_path = file_path + date + "_.csv"
df_Part_21_mod.to_csv(file_path, index=False)

root.mainloop()
root.withdraw()
