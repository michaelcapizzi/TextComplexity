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

As a result of the small data size, leave-one-out testing was utilized.

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

**The base line used is a constricted version of the popular Lexile measure.**

![alt tag](https://cloud.githubusercontent.com/assets/8990766/7360833/6f1ba746-ed04-11e4-974e-8d01e02f3b33.png)

Lexile officially presents a score-band for each grade level with significant overlap between grade levels (see above), which isn't very useful. In order to generate an equivalent score for Lexile to this system, the Lexile has been collapsed to the *lowest* grade-level in the CSSS band.  For example, a Lexile score of `1010` can fall anywhere from 5th to 8th grade.  For this comparison, a score of `1010` will be assigned to the `4-5` band.

 Features Used | Model Used | Accuracy | Precision | Recall | F1
 ---|---|---|---|---|---
  | *Lexile* | *24.1%* | *.32* | *.36* | *.34*
  | | | | |
 **Lexical** | **Random Forest** | **40.7%** | **.39** | **.38** | **.38**
 | Perceptron | 31.5% | .11 | .20 | .14
 | Logistic Regression | 3.3% | .27 | .25 | .26
 | Linear SVM | 33.3% | .28 | .28 | .28
 | | | | |
 | Random Forest | 24.1% | .17 | .18 | .17
 | Perceptron | 24.1% | .15 | .17 | .16
Syntactic | Logistic Regression | 35.2% | .28 | .30 | .29
 | Linear SVM | 25.9% | .22 | .22 | .22
 | | | | |
 | Random Forest | 22.2% | .17 | .16 | .16
 | Perceptron | 18.5% | .03 | .13 | .05
 Paragraph | Logistic Regression | 24.1% | .30 | .19 | .23
 | Linear SVM | 12% | .13 | .12 | .12
 | | | | |
 | Random Forest | 31.5% | .25 | .25 | .25
 | Perceptron | 29.6% | .14 | .23 | .18
 Lex + Syn | Logistic Regression | 42.6% | .36 | .35 | .35
 | Linear SVM | 38.9% | .34 | .32 | .33
 | | | | |
 | Random Forest | 31.5% | .24 | .25 | .25
 | Perceptron | 22.2% | .08 | .15 | .11
 Lex + Par | Logistic Regression | 37% | .30 | .29 | .30
 | Linear SVM | 29.7% | .25 | .24 | .24
 | | | | |
 | Random Forest | 27.7% | .24 | .22 | .23
 | Perceptron | 24.1% | .13 | .18 | .15
 Syn + Par | Logistic Regression | 33.3% | .28 | .28 | .28
 | Linear SVM | 27.7% | .28 | .27 | .27
 | | | | |
 | Random Forest | 33.3% | .28 | .28 | .28
 | Perceptron | 22.2% | .22 | .16 | .18
 Lex + Syn + Par | Logistic Regression | **40.7%** | .32 | .32 | .32
 | Linear SVM | 33.3% | .28 | .26 | .27
 
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
   | Random Forest | 61.1% | .52 | .49 | .50
Lexical | Perceptron | 64.8% | .52 | .52 | .52
  | Logistic Regression | 68.5% | .46 | .51 | .49
  | Linear SVM | 64.8% | .52 | .51 | .51
  | | | | |
  | Random Forest | 70.4% | .46 | .52 | .49
Syntactic | Perceptron | 61.1% | .61 | .59 | .60
  | Logistic Regression | 61.1% | .57 | .52 | .54
  | Linear SVM | 57% | .50 | .48 | .49
  | | | | |
  | Random Forest | 64.8% | .64 | .54 | .59
  | Perceptron | 53.7% | .47 | .45 | .46
Paragraph | Logistic Regression | 66.7% | .77 | .53 | .63
  | Linear SVM | 50% | .49 | .51 | .59
  | | | | |
Lex + Syn | Random Forest | 68.5% | .47 | .51 | .49
  | Perceptron | 61.1% | .55 | .54 | .54
  | Logistic Regression | 61.1% | .50 | .49 | .49
  | Linear SVM | 64.8% | .55 | .55 | .55
  | | | | |
  | Random Forest | 68.5% | .63 | .59 | .61
  | Perceptron | 68.5% | .65 | **.65** | .65
**Lex + Par** | Logistic Regression | 70.4% | .62 | .60 | .61
  | **Linear SVM** | **72.2%** | .63 | .62 | **.62**
  | | | | |
Syn + Par | Random Forest | 72.2% | .65 | .57 | .61
  | Perceptron | 38.9% | .49 | .42 | .45
  | Logistic Regression | 59.3% | .55 | .47 | .51
  | Linear SVM | 55.6% | .53 | .49 | .51
  | | | | |
Lex + Syn + Par | Random Forest | 68.5% | **.78** | .55 | .65
  | Perceptron | 59.2% | .61 | .63 | .62
  | Logistic Regression | 66.7% | .61 | .56 | .58
  | Linear SVM | 64.8% | .57 | .54 | .56