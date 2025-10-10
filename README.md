# projects_portfolio

Some projects developed during my university studies:

## Big Data in Health Care

Project for the Big Data in Health Care course, academic year 2022/23, Master’s Degree in Data Science, University of Milano – Bicocca.

The project aims to analyze the incidence of second recurrence in patients with surgical resection of hepatocellular carcinoma (HCC) who have already experienced a first recurrence.

HCC is a malignant liver tumor, and surgical resection is often used as a treatment option. However, even after tumor removal, recurrence is common. This study focuses on patients who have already experienced one recurrence.

Data analysis was carried out using non-parametric and univariate methods, such as the Cox model. The ultimate goal of the project is to develop a predictive model that can help identify patients at risk of a second recurrence, allowing appropriate preventive measures to be taken.

## Data_Management

Project for the Data Management course, academic year 2022/23, Master’s Degree in Data Science, University of Milano – Bicocca.

The aim of the project was to create a dataframe by selecting different data sources (in our case, three were used: https://fbref.com/en/, https://www.transfermarkt.it/, and https://www.capology.com/). The data were imported through downloads using the worldfootballR library for the first two sources, and via web scraping with Python’s BeautifulSoup for the third.

The project involved integrating, cleaning, and standardizing the data, loading them into a DBMS (MongoDB), and finally running queries on the integrated dataset.

The resulting dataset combined information from different sources about eight football leagues (Serie A, Premier League, Ligue 1, Bundesliga, La Liga, Eredivisie, Liga Nos, and MLS) for the 2022/23 season, covering player contracts, market values, and performance statistics.

## Data_Science_Lab

Project for the Data Science Lab course, academic year 2022/23, Master’s Degree in Data Science, University of Milano – Bicocca.

The goal of the project was to explore a dataset containing revenues and receipt counts from six restaurants (located in Lombardy and Emilia-Romagna) between January 2018 and April 2022, in order to gain meaningful insights.

After an initial phase of importing, cleaning, and processing the provided datasets, additional data sources related to the studied phenomenon were integrated, followed by data exploration.

The second phase of the project focused on building time series models to represent the revenue trends of the six restaurants, identifying seasonal patterns, significant trends, and insights useful for estimating losses due to COVID-19 and related lockdown restrictions.

## Data Visualization

Project for the Data Visualization course, academic year 2022/23, Master’s Degree in Data Science, University of Milano – Bicocca.

The interactive visualization, created with Tableau Public, is available at the following link:
Tableau Visualization

Tableau Public was used to create the infographics for the project.

The dataset was built by integrating multiple datasets from fbref.com, particularly focusing on general and possession-related statistics from the top 5 European football leagues (Serie A, Premier League, Ligue 1, Bundesliga, and La Liga) for the 2017/18–2021/22 seasons.

## Data Mining

Project for the Data Mining course, academic year 2020/21, Bachelor’s Degree in Statistics and Information Management (SGI), University of Milano – Bicocca.

The goal of this study was to build a classifier capable of predicting user ratings based on the characteristics of a mobile application.

##Foundations of Computer Science

Project for the Foundations of Computer Science course, academic year 2021/22.

Assignment
You have to work on the following files:
- Books
- Book ratings
- Users
- Goodbooks books
- Goodbooks ratings

Notes:
It is mandatory to use GitHub for project development.
The project must be a Jupyter Notebook.
There are no restrictions on the Python version or libraries used.
To read the files, use encoding='latin-1'.
All questions must be asked publicly on Zulip; otherwise, no answers will be provided.

Tasks:
Normalize the “location” field in the Users dataset, splitting it into city, region, and country.
Compute the average rating for each book in both Books and GoodBooks datasets.
Merge rows with the same title, author, and publisher into a dataset called merged_books.
Compute average, min, and max ratings for merged books.
Compute text review counts shared among authors.
Identify authors with the highest shared review counts per publication year.
Compare average ratings across datasets by ISBN.
Split users by age group (unknown, 0–14, 15–24, etc.).
Identify books that appear only in GoodBooks.
Find books and authors appearing most frequently and with the highest average rating.

## R Laboratory for Biostatistics

Project for the R Laboratory for Biostatistics course, academic year 2021/22, Master’s Degree in Data Science, University of Milano – Bicocca.

Cleaning of the initial clinical dataset, application of machine learning techniques, and descriptive analysis of relevant variables.

## Machine Learning

Project for the Machine Learning course, academic year 2021/22, Master’s Degree in Data Science, University of Milano – Bicocca.

Development of a KNIME workflow for a machine learning project based on a chosen dataset.

## Statistical Models
Project completed using RMarkdown during the Statistical Models exam, academic year 2019/20, Bachelor’s Degree in Statistics and Information Management (SGI), University of Milano – Bicocca.

Assignment summary:
Analysis of restaurant meal costs as a function of food quality, elegance, service, and location.
Tasks included descriptive statistics, scatter plots, multicollinearity checks, multiple regression modeling, interpretation of parameters, residual analysis, confidence intervals, and application of the Akaike Information Criterion for variable selection.

## Social Media Analytics

Project for the Social Media Analytics course, academic year 2022/23, Master’s Degree in Data Science, University of Milano – Bicocca.

The project involved implementing sentiment analysis, topic modeling, and community detection to uncover the main themes, key members, and sentiment of posts from the subreddit r/ACMilan.

Steps included text preprocessing (normalization, stopword removal, tokenization, lemmatization), exploratory data analysis, topic modeling (with wordcloud creation), sentiment analysis (using three approaches: AFINN Lexicon, Opinion Lexicon, and Dictionary-Based Emotion Detection), and community detection through graph-based network metrics.

## Streaming Data Management and Time Series Analysis

Project for the Streaming Data Management and Time Series Analysis course, academic year 2022/23, Master’s Degree in Data Science, University of Milano – Bicocca.

The project focused on implementing algorithms to forecast values in a time series of carbon monoxide (CO) measurements collected between 01/01/2017 and 30/12/2017.
Since the dataset only included data until 30/11/2017, the goal was to predict the missing final month.

Three types of algorithms were evaluated — ARIMA, UCM, and machine learning models — to identify the best predictive performance using Mean Absolute Error (MAE) as the evaluation metric.

## Bachelor’s Thesis

Bachelor’s thesis titled “Automated analysis and visualization techniques of data generated with flow cytometry platforms.”

The thesis focused on the development and application of flow cytometry techniques for biological and clinical data analysis.

During an internship at the IRCCS National Institute for Cancer Research, I explored data analysis methods based on flow cytometry under the supervision of Dr. Luca Lalli and Dr. Luigi Mariani.
The work involved applying statistical methods (using R), reviewing scientific literature, and collaborating with researchers and clinicians on biomedical data analysis.

## Master’s Thesis

Master’s thesis titled “Analysis of clinical data related to extended neonatal screening and design of big data analysis and machine learning methodologies with Python for the prediction of metabolic diseases in the pediatric population of Lombardy.”

Developed within the Buzzi Project (ASST Fatebenefratelli-Sacco Hospital, Milan), the thesis analyzed one of Europe’s largest extended neonatal screening databases — a test introduced in the 1990s across Italy to detect metabolic and clinical disorders in newborns.

The project aimed to design and evaluate synergistic big data and machine learning methods for predictive clinical modeling.

Conducted as an internship at the MUDI Lab (University of Milano – Bicocca) under Prof. Federico Cabitza and Dr. Luca Marconi, the project involved Python-based data import, cleaning, exploration, dimensionality reduction (UMAP, PCA, t-SNE), clustering (hierarchical, BIRCH, DBSCAN, K-means), and in-depth results analysis.

## Text Mining and Search

Project for the Text Mining and Search course, academic year 2022/23, Master’s Degree in Data Science, University of Milano – Bicocca.

The project implemented topic modeling and text classification algorithms to uncover the main themes within user reviews, grouping them into distinct categories.

Steps included text preprocessing (normalization, stopword removal, tokenization, lemmatization). LDA was used for topic modeling, while two text representations (TF-IDF and Doc2Vec) were evaluated for the classification task.
