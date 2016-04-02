Data
-----------------

The data is sparse, consisting of full-texts and excerpts from different grade-level bands as identified by the Common Core Standards:

Grade-Level Band | # of Full-text Samples | # of Excerpt Samples | Total # of Samples
---|---|---|---
K-1 | 0 | 3 | 3
2-3 | 1 | 6 | 7
4-5 | 2 | 6 | 8
6-8 | 3 | 3 | 6
9-10 | 8 | 4 | 12
11-12 | 7 | 8 | 16

As a result of the small data size, leave-one-out testing was be utilized.

Features Collected
---------------------

Lexical Features | Syntactic Features | Paragraph Features
---|---|---
\# of distinct conjunctions used\* | average \# of conjunctions per sentence | length of paragraphs
% of distinct nouns used\* | sentence length in text | \# of relations per paragraph
% of distinct verbs used\* | parse tree size in text | % of relations in each direction
% of distinct adjectives used\* | parse tree depth in text | % of each relation type
% of tokens not present in concreteness database | distance to verb in sentences in text |  
concreteness score present in text | \# of constituents in a sentence in text | 
concreteness score of most-used noun in text | constituent length in text | 
concreteness score of most-used verb in text | \# of clauses per sentence in text | 
concreteness score of most-used adjective in text | % of simple sentences in text | 
 | % of complex sentences in text
 | % of compound sentences in text |
 | % of compound-complex sentences in text |
 | % of fragments in text |