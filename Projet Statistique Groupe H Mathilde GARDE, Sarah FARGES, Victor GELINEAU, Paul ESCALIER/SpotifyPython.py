import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.gridspec as gs
import seaborn as sns
sns.set()
import warnings
warnings.filterwarnings('ignore')

df = pd.read_csv("C:/Users/escal/OneDrive/Bureau/R/songs_normalize.csv")
key_mode = df.groupby(['key','mode']).size().unstack(fill_value = 0).reset_index()
key_mode.rename(columns = {0: 'mineur', 1: 'majeur'}, inplace = True)
key_mode.key.replace({0:'C',1:'C♯/Db',2:'D',3:'Eb/D♯',4:'E',5:'F',6:'F♯/Gb',7:'G',8:'Ab/G♯',9:'A',10:'Bb/A♯',11:'B'}, inplace=True)
key_mode
key_mode.plot.bar(x='key',stacked=True,color=['blue','darkblue'],title="Distribution des titres selon leur clé majeur ou mineur",figsize=(14,8),fontsize=12)
plt.show()
