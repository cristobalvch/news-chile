import requests
from bs4 import BeautifulSoup
from IPython.display import clear_output

import pandas as pd
import time
import datetime
import re
import unidecode


def webscraping_cnn(seccion,pages):
    titles = []
    descriptions = []
    news_url = []
    days = []
    months = []
    years = []
    hours = []

    for i in range(1,pages+1):

        url = 'https://www.cnnchile.com/category/'+seccion+'/page/'+str(i)+'/'
        response = requests.get(url)
        soup = BeautifulSoup(response.text,'html.parser')
        containers = soup.find_all('div',class_='inner-item__content')
        for container in containers:
            titles.append(container.find('h2',class_='inner-item__title').text)
            descriptions.append(container.find('div',class_='inner-item__text').find('p').text)
            news_url.append(container.find('h2',class_='inner-item__title').find('a')['href'])

        print(f"titles:+{len(titles)} description:+{len(descriptions)} urls:+{len(news_url)}")
        clear_output(wait=True)
        time.sleep(0.8)

    
    titles = [x.replace("\n","") for x in titles]
    dates = [x.split("/")[0] for x in descriptions]
    summary = [x.split("/")[1] for x in descriptions]

    for date in dates:

        if len(date.split("-"))==3:
            days.append(date.split("-")[0])
            months.append(date.split("-")[1])
            years.append(date.split("-")[2].split(" ")[0])
            hours.append(date.split("-")[2].split(" ")[1])
        
        else:
            days.append(str(datetime.date.today()).split("-")[2])
            months.append(str(datetime.date.today()).split("-")[1])
            years.append(str(datetime.date.today()).split("-")[0])
            hours.append(date)



    df = pd.DataFrame({'year':years,'month':months,'day':days,'hours':hours,'title':titles,'summary':summary,'url':news_url})
    return df

def clean_text(df,column):

    for i in df.index:
        text = unidecode.unidecode(str(df[column].iloc[i]))
        text1 = text.lower()
        text1 = re.sub('\[.*?¿\]\%', ' ', text1)
        text1 = re.sub('[‘’“”…«»]', '', text1)
        text1 = re.sub('\w*\d\w*', '', text1)
        text1 = re.sub('[‘’“”…«»]', '', text1)
        text1 = re.sub('\n', ' ', text1)
        text1 = text1.strip()
        text1 = text1.replace("?","").replace("$.","")


        df[column].iloc[i] = text1

    return df[column]