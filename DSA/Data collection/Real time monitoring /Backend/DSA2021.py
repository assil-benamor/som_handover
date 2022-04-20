#!/usr/bin/env python
# coding: utf-8

# In[929]:


import requests
import pandas as pd 
import csv
import io
import mysql.connector as connection
import pandas.io.sql as psql
import re

pd.set_option('mode.chained_assignment', None)


# ## Kobo server credentials

# In[930]:


kobo_server_url = "https://kobo.humanitarianresponse.info/"
url = "https://kc.humanitarianresponse.info/api/v1/data.csv"
kobo_user = "reachsomalia_do"
kobo_pw = "hypegrammar_magic"
form_id = "929319"
dataurl = "https://kc.humanitarianresponse.info/api/v1/data/"+form_id+".json"


# ## Connect to Kobo server and pull the sureveys
# 

# In[931]:


urlData = requests.get(url = dataurl, auth=(kobo_user, kobo_pw)).content


# In[932]:


#re.sub(r'(begin_group[^\/]*|b_localisation_idp_site)', 'begin_group', "b_localisation/b_localisation_idp_site/idp_code")


# In[933]:


## correcting the random group names
jsonObj = re.sub(r'(begin_group[^\/]*|b_localisation_idp_site)', 'begin_group', urlData.decode('utf-8'))


# In[934]:


df = pd.read_json(io.StringIO(jsonObj))
print(df.shape[0])


# In[935]:


df[['b_localisation/begin_group/idp_code','_uuid']]


# ## Connect to the SQL server and pull the idp sites database

# In[936]:


try:
    mydb = connection.connect(host="165.232.64.110", database = 'dsa',user="user", passwd="--lilos@404@Sql__",use_pure=True)
    query = "Select * from idp;"
    result_dataFrame = pd.read_sql(query,mydb)
    mydb.close() 
    #result_dataFrame.rename(columns = {'\ufeffpcode': 'pcode'}, inplace = True)
except Exception as e:
    mydb.close()
    print(str(e))


# In[937]:


result_dataFrame.head(7)


# ## Connect to the SQL server and pull the done database

# In[938]:


try:
    mydb = connection.connect(host="165.232.64.110", database = 'dsa',user="user", passwd="--lilos@404@Sql__",use_pure=True)
    query = "Select * from done;"
    df_done = pd.read_sql(query,mydb)
    mydb.close() 
except Exception as e:
    mydb.close()
    print(str(e))


# In[939]:


df = df[~df._uuid.isin(df_done["_uuid"])]


# In[940]:


df_to_update = df[['b_localisation/begin_group/idp_code','_uuid']]


# In[941]:


df_to_update.rename(columns = {'b_localisation/begin_group/idp_code': 'pcode'}, inplace = True)
df_to_update = df_to_update.merge(result_dataFrame, on='pcode', how='left')
df_to_update


# In[942]:


df2 = df_to_update.groupby('pcode').agg({'times': 'count'}).rename(columns = {'times': 'times2'}, inplace = False)


# In[943]:


df_to_update = df_to_update.merge(df2, on='pcode', how='left')


# In[944]:


df_to_update['times'] += df_to_update['times2']


# In[945]:


df_to_update.drop('times2', inplace=True, axis=1)


# In[946]:


df_to_update


# ## Update the number of times visited per site

# In[947]:


if (df_to_update.shape[0]!=0):   
    try:
        mydb = connection.connect(host="165.232.64.110", database = 'dsa',user="user", passwd="--lilos@404@Sql__",use_pure=True)
        mycursor = mydb.cursor()
        for index, row in df_to_update.iterrows():
            sql = "UPDATE idp SET times = %s, visited = 1 WHERE pcode = %s"
            val = (str(row['times']), str(row['pcode']))
            mycursor.execute(sql, val)
        mydb.commit()
        mydb.close() 
        print(df_to_update.shape[0], "record inserted.")
    except Exception as e:
        mydb.close()
        print(str(e))


# ## Append the new uuids to the "done" table 

# In[948]:


if (df_to_update.shape[0]!=0): 
    try:
        mydb = connection.connect(host="165.232.64.110", database = 'dsa',user="user", passwd="--lilos@404@Sql__",use_pure=True)
        mycursor = mydb.cursor()
        for index, row in df_to_update.iterrows():
            #print(index)
            #print(row['_uuid'])
            sql = "INSERT INTO done (id,_uuid) VALUES (%s,%s)"
            val = (str(df_done.shape[0]+index+1),str(row['_uuid']))
            mycursor.execute(sql, val)
        mydb.commit()
        mydb.close() 
        print(df_to_update.shape[0], "record inserted.")
    except Exception as e:
        mydb.close()
        print(str(e))


# In[ ]:





# In[ ]:




