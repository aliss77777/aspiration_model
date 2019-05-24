# aspiration_model
Performing social network analysis and gathering community insights using the Watson Personality insights API. This is quite a robust technique that includes many steps - a powerpoint example of the final output is included for reference.

This technique has a few steps.

0. Pre-requisite is acquiring data via the Twitter API or social listening platform (not shown)
1. Creating an Adjacency List
2. Using Gephi to append modularity class to the adjacency list (output of Gephi analysis included)
  - Gephi is an open-source tool and can be freely downloaded
3. Creating summary statistics to understand what kind of conversations comprise each cluster 
4. Loading the conversations of each cluster through the Watson Personality Insights API and returning CSV outpus
5. Loading the data files into a pre-built Tableau workbook for visualization and interpretation
  - Tableau license required; although you can download a free trial for ~2 weeks
