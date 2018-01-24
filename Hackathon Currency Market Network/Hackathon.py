
# coding: utf-8

# In[1]:

import pandas as pd
import numpy as np
import matplotlib.pylab as plt
get_ipython().magic('matplotlib inline')
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 15, 6
import statsmodels.api as sm
from statsmodels.tsa.api import VAR, DynamicVAR


# In[2]:

currencies = pd.read_csv('currencies_2.csv')
financial = pd.read_csv('financial_2.csv')
financial.head()


# In[3]:

# currencies.groupby('Ticker')

# for title, group in currencies.groupby('Ticker'):
#     group.plot(x='date', y='PX_LAST', title=title)


# In[4]:

# financial.groupby('Ticker')

# for title, group in financial.groupby('Ticker'):
#     group.plot(x='date', y='PX_LAST', title=title)


# In[5]:

currencies['date'] = pd.to_datetime(currencies.date)
financial['date'] = pd.to_datetime(financial.date)
# print(currencies['date'])


# In[6]:

p_currencies = currencies.pivot(index='date',columns='Ticker',values='PX_OPEN')
# p_currencies.head()


# In[7]:

p_fin = financial.pivot(index='date',columns='Ticker',values='PX_OPEN')
# p_fin.head()


# In[8]:

p_currencies = p_currencies.bfill()
p_fin = p_fin.bfill()

# p_fin.head()


# In[9]:

p_fin_diff = p_fin.diff()
p_currencies_diff = p_currencies.diff()


# In[10]:

p_fin_diff = p_fin_diff.iloc[1:] 
p_currencies_diff = p_currencies_diff.iloc[1:]


# In[11]:

p_currencies_shortlist = p_currencies_diff.filter(['USDBRL Curncy', 'USDMXN Curncy', 'USDCAD Curncy', 'USDCLP Curncy', 'USDEUR Curncy', 'USDGBP Curncy', 'USDSEK Curncy', 'USDCHF Curncy', 'USDJPY Curncy', 'USDHKD Curncy', 'USDCNY Curncy', 'USDAUD Curncy', 'USDKRW Curncy', 'USDINR Curncy', 'USDTWD Curncy'], axis=1)
# p_currencies_shortlist.head()


# In[ ]:




# In[44]:

p_data = p_fin_diff.join(p_currencies_shortlist)
p_data.info()
p_data = p_data.bfill()


# In[45]:

p_data_training = p_data.iloc[:2600]
p_data_test = p_data.iloc[2601:]


# In[46]:

model = VAR(p_data_training)
# print(type(model))


# In[47]:

results = model.fit()
results.summary()


# In[48]:

def get_p(name1, name2):
    temp_dict = results.test_causality(name1, [name2], kind='f')
    if (temp_dict["pvalue"] > .05):
        return 0
    else:
        return 1


# In[80]:

B = np.zeros((len(p_data_training.columns),len(p_data_training.columns)))
for i in range(0,len(p_data_training.columns)):
    for j in range(0,len(p_data_training.columns)):
        B[i,j] = get_p(p_data_training.columns[i], p_data_training.columns[j])

    


# In[50]:

np.savetxt('adjancency.csv', B, delimiter=',')


# In[51]:

import networkx as nx

def show_graph_with_labels(adjacency_matrix, mylabels):
    rows, cols = np.where(adjacency_matrix == 1)
    edges = zip(rows.tolist(), cols.tolist())
    gr = nx.Graph()
    gr.add_edges_from(edges)
    nx.draw(gr, node_size=500, labels=mylabels, with_labels=True)
    plt.show()

show_graph_with_labels(B, p_data_training.columns)


# In[65]:

lag_order = results.k_ar

forecasts = results.forecast(p_data_training.values[-lag_order:], 31)


# In[72]:

residuals = p_data_test.values - forecasts


# In[77]:

plt.hist(residuals)


# In[78]:

results.plot_forecast(31)

