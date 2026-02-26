import delimited "C:\Users\czs792\Dropbox\Teaching\SoDA 502 Fall 19\Code and Data\Black Names ML Data.csv", clear
drop if comment==""

python:
from sfi import Data
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
import pandas as pd
import string

df = pd.read_csv(r'C:\Users\czs792\Dropbox\Teaching\SoDA 502 Fall 19\Code and Data\Black Names ML Data.csv')

analyzer = SentimentIntensityAnalyzer() 
def sentiment_analyzer(comment):
    comment = comment.translate(str.maketrans('','',string.punctuation))
    comment = comment[:2000]
    return analyzer.polarity_scores(comment)['compound']

df = df.dropna(subset=['comments'])
df = df.reset_index()

df['sentiment'] = df['comments'].apply(sentiment_analyzer)

Data.addVarFloat('sentiment')
Data.store('sentiment', None, df.sentiment)
end

gsort -sentiment
reg sentiment black_name educ_low
