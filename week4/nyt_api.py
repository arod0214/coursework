#
# file: get_article_urls.py
#
# description: fetches article urls from the NYTimes API
#
# usage: get_articles.py <api_key>
#
# requirements: a NYTimes API key
#   available at https://developer.nytimes.com/signup
#

import requests
import json
import time
import pandas as pd

ARTICLE_SEARCH_URL = 'https://api.nytimes.com/svc/search/v2/articlesearch.json'

api_key = <your api_key>
name = ['Business', 'World']  # want business and world articles

nyt_busi_world = pd.DataFrame() # initialize dataframe and dictionary 
data = {}
for i in name:
    for j in range(0,100): # 1000 articles
       params = {'api-key': api_key, 'fq':"section_name:"+i, 'sort':"newest", 
                  'fl': "section_name, web_url, snippet, pub_date", 'page':j} # only saving information per article I want
       r = requests.get(ARTICLE_SEARCH_URL, params)
       time.sleep(1)
       data[i] = json.loads(r.content)
       nyt_busi_world = nyt_busi_world.append(data[i]['response']['docs']) # takes keys as column names and organizes data into df
    
nyt_busi_world.to_csv('nyt_busi_world_articles.csv', encoding='utf-8') # still does not decode properly, not sure why
        
 
