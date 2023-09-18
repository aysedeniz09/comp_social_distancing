### Install packages first time running the script

``` python
# PyPI link: https://pypi.org/project/nltk/
!pip install nltk

# PyPI link: https://pypi.org/project/newspaper3k/
!pip install newspaper3k

# PyPI link: https://pypi.org/project/pandas/
!pip install pandas
```

### Load the needed libraries

``` python
# Import the nltk library for natural language processing tasks
import nltk

# Download the 'punkt' tokenizer models from nltk (used for text tokenization)
nltk.download('punkt')

# Import the newspaper library for web scraping and article extraction
import newspaper

# Import the pandas library for data manipulation and analysis
import pandas as pd

# Import the csv library for reading and writing CSV files
import csv

# Import the Article class from newspaper for working with articles
from newspaper import Article

# Import the fulltext module from newspaper for extracting full text from articles
from newspaper import fulltext

# Import the pprint module for pretty-printing data structures
from pprint import pprint

# Import the random.randint function for generating random integers
from random import randint

# Import the os library for operating system-related functionality
import os

# Import the time and sleep functions for handling time-related operations
import time
from time import sleep

```

### Load the csv file with links

``` python
dfmain = pd.read_csv (r'Scrape_1.csv')
dfmain.info()
```

### Change the urls into a list

``` python
list_of_urls = dfmain['link'].tolist()
```

### Run the scraping loop you can change what you select from this documentation <https://newspaper.readthedocs.io/en/latest/>

``` python
rows = []
for link in list_of_urls:
    try:
        a = Article(url="%s" % (link), language='ru') ## change language
        a.download()
        a.parse()
         
        author = a.authors
        text = a.text
        title = a.title
        
        
        row = {'url':link,
               'author':author,
               'text':text,
               'title': title}
        
        rows.append(row)
    except Exception as e:
        print(e)
        row = {'url':link,
        'author':'N/A',
        'text':'N/A',
        'title': 'N/A'}
        
        rows.append(row)

df_v1 = pd.DataFrame(rows)
```

#### Save the raw file to csv as a backup

``` python
df_v1.to_csv('my_scraped_articles_raw_russian.csv')
```

### Join with the original file

``` python
dfmaster = dfmain.merge(df_v1, left_on='link', right_on='url') # we changed to df to sample, when running full dont forget to change it to main
dfmaster.to_csv('my_scraped_articles_russian_master_v1.csv')
```

