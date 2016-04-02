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
\# of distinct conjunctions used | average \# of conjunctions per sentence | length of paragraphs
% of distinct nouns used | sentence length | \# of relations per paragraph
% of distinct verbs used | parse tree size | % of relations in each direction
% of distinct adjectives used | parse tree depth | % of each relation type
% of tokens not present in concreteness database | distance to verb in sentences  |  
concreteness score present in text | \# of constituents in a sentence | 
concreteness score of most-used noun in text | constituent lengths | 
concreteness score of most-used verb in text | \# of clauses per sentence | 
concreteness score of most-used adjective in text | % of simple sentences | 
 word length | % of complex sentences |
 distinct word ratio | % of compound sentences |
 | % of compound-complex sentences |
 | % of fragments |
 | % of independent clauses | 
 | % of dependent clauses | 
 | amount of surplus punctuation | 
 | clause tree sizes | 
 | clause tree depths | 
 | sentence coherence |
 

Results for 6-class classification
--------------

**RandomForest**: 
- number of trees= 1000
- maximum tree depth = 10
- features used for each tree = 20%
- number of threads = 3

**Perceptron**: 
- epochs = 20
- features used for each iteration = 100%
    
**LogisticRegression**: 
- bias = false

**Linear SVM**: 
-bias = false

K-1 | 2-3 | 4-5 | 6-8 | 9-10 | 11-12
---|---|---|---|---|---

**The "features used" label is on the row of the model with the best F1 performance for that feature set.**
**The best scores across all feature sets and all models are in bold**

 Features Used | Model Used | Accuracy | Precision | Recall | F1
 ---|---|---|---|---|---
 | Random Forest | 55.6% | .48 | .47 | .47
 | Perceptron | 14.8% | .13 | .17 | .15
 | Logistic Regression | 55.6% | .37 | .38 | .38
Lexical | Linear SVM | 63% | .51 | .50 | .50
 | | | | |
 | Random Forest | xx% | xx | xx | xx
 | Perceptron | xx% | xx | xx | xx
Syntactic | Logistic Regression | xx% | xx | xx | xx
 | Linear SVM | xx% | xx | xx | xx
 | | | | |
 Paragraph | Random Forest | 55.6% | .37 | .41 | .39
 | Perceptron | 16.7% | .09 | .17 | .12
 | Logistic Regression | 53.7% | .45 | .38 | .41
 | Linear SVM | 50% | .32 | .34 | .33
 | | | | |
 | Random Forest | xx% | xx | xx | xx
 | Perceptron | xx% | xx | xx | xx
 Lex + Syn | Logistic Regression | xx% | xx | xx | xx
 | Linear SVM | xx% | xx | xx | xx
 | | | | |
 Lex + Par | Random Forest | 57.4% | .46 | .45 | .46
 | Perceptron | 16.7% | .30 | .17 | .22
 | Logistic Regression | 55.6% | .35 | .41 | .37
 | Linear SVM | 55.6% | .40 | .41 | .41
 | | | | |
 | Random Forest | xx% | xx | xx | xx
 | Perceptron | xx% | xx | xx | xx
 Syn + Par | Logistic Regression | xx% | xx | xx | xx
 | Linear SVM | xx% | xx | xx | xx
 | | | | |
 | Random Forest | xx% | xx | xx | xx
 | Perceptron | xx% | xx | xx | xx
 Lex + Syn + Par | Logistic Regression | xx% | xx | xx | xx
 | Linear SVM | xx% | xx | xx | xx
 
 
Results for 3-class classification
-------------------------------------
 
 **RandomForest**: 
 - number of trees= 1000
 - maximum tree depth = 5
 - features used for each tree = 20%
 - number of threads = 3
 
 **Perceptron**: 
 - epochs = 20
 - features used for each iteration = 100%
     
 **LogisticRegression**: 
 - bias = false
 
 **Linear SVM**: 
 -bias = false
 
 K-5 | 6-8 | 9-12
 ---|---|---
 
  Features Used | Model Used | Accuracy | Precision | Recall | F1
  ---|---|---|---|---|---
  | Random Forest | 63% | .58 | .50 | .54
 Lexical | Perceptron | 64.8% | .62 | .65 | .63
  | Logistic Regression | 68.5% | .45 | .52 | .48
  | Linear SVM | 68.5% | .59 | .57 | .58
  | | | | |
  | Random Forest | xx% | xx | xx | xx
  | Perceptron | xx% | xx | xx | xx
 Syntactic | Logistic Regression | xx% | xx | xx | xx
  | Linear SVM | xx% | xx | xx | xx
  | | | | |
  Paragraph | Random Forest | 73.6% | .69 | .66 | .67
  | Perceptron | 60.4% | .56 | .58 | .57
  | Logistic Regression | 67.9% | .45 | .51 | .47
  | Linear SVM | 70% | .55 | .55 | .55
  | | | | |
  | Random Forest | xx% | xx | xx | xx
  | Perceptron | xx% | xx | xx | xx
  Lex + Syn | Logistic Regression | xx% | xx | xx | xx
  | Linear SVM | xx% | xx | xx | xx
  | | | | |
  | Random Forest | 72.2% | .70 | .62 | .66
  | Perceptron | 55.6% | .52 | .47 | .50
  Lex + Par | Logistic Regression | 75.9% | .83 | .64 | .72
  | Linear SVM | 70.4% | .61 | .61 | .61
  | | | | |
  | Random Forest | xx% | xx | xx | xx
  | Perceptron | xx% | xx | xx | xx
  Syn + Par | Logistic Regression | xx% | xx | xx | xx
  | Linear SVM | xx% | xx | xx | xx
  | | | | |
  | Random Forest | xx% | xx | xx | xx
  | Perceptron | xx% | xx | xx | xx
  Lex + Syn + Par | Logistic Regression | xx% | xx | xx | xx
  | Linear SVM | xx% | xx | xx | xx