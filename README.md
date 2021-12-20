# Final data science project

We used metadata in rdf form to analyze the prevalence of gender in 54517 datasets from opengov.de. The data was retrieved through an API and cleaned. For the analysis different parts of the dataset were subset in order to filter for gendered data. Four different data analysis approaches were performed: calculating the percentage for the overall datsets and gendered data there of and comparing on an annual basis,filtering the amount of gendered and none gendered datasets per topic, unsupervised topic modeling with LDA on the keywords, term frequency analysis on the keywords. We found that gendered datasets only make up a small amount (~10 percent) of overall datasets and are confined to 7 out of 12 topics. Furthermore, the most prevalent data appears to be treating construction and traffic. There is still a considerable gender data gap on this portal. (HERE I WOULD TRY TO FIND A BETTER ENDING)

- and maybe the link to the actual article/HTML

## Data folder

- Contains the raw data we extarcted from the API on ['govdata.de'](https://www.govdata.de/)
- Contains the cleaned dataset we used for further analysis

## Scripts folder - functionality

### 00-wrangling
- API query
- Selecting the metadata used for the project
- Join them in a dataframe

### 001-data-cleaning
- Removing punctuation, lower

### 002-topic-modelling
- Cleaning data for LDA algorithm
- Applying LDA algorithm

### 003-tables-visualisations
- Visualizing accumulation of datasets over time
- Visualizing gendered data presence in topics

### 004-term_freq
- Visualizing term frequency within the datasets' keywords

## Individual contributors

* Dinah Rabe(d.rabe@mpp.hertie-school.org)

* Helena Bakic(xxx)

* Anna-Lisa Wirth (213286@mds.hertie-school.org)

## License

The material in this repository is made available under the [MIT license](http://opensource.org/licenses/mit-license.php).