"""
Creates Directories for Results Storing and Mangement
"""
import pandas as pd
import os

filenames_excel = pd.read_csv('file_name.csv')

parent_dir = "/Users/elvisyung/Desktop/t1-research/results_blah/"
for i in range(len(filenames_excel.index)):
    directory = filenames_excel.file_name[i] + '_file_extension_name'
    path = os.path.join(parent_dir, directory)
    os.mkdir(path)
