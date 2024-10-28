# README

## 1 Group Members
- Sheridan Heywood
- Vivek Paligadu
- Eli Miller 
- Daniel Tse

## 2 Packages

The following code is taken from the start of the main report, which imports pandas, scikit learn and matplotlib for part of the analysis, and ucimlrepo for importing the dataset as instructed by its webpage https://archive.ics.uci.edu/dataset/2/adult 

import sys
!{sys.executable} -m pip install pandas
!{sys.executable} -m pip install scikit-learn
!{sys.executable} -m pip install ucimlrepo
!{sys.executable} -m pip install matplotlib
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.model_selection import *
from sklearn import svm
from sklearn import metrics
from sklearn.preprocessing import OneHotEncoder
from sklearn.inspection import permutation_importance
from ucimlrepo import fetch_ucirepo 
