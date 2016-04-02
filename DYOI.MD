# Commentary to accompany Text Complexity Code

I designed this to be my interview because I wanted to highlight how much my coding ability (in general, and in `scala` specifically) has improved since I first designed this project.  When I first built this system, I had only been coding in `scala` for less than six months.  It is now a year later, and so this entire system has been built with less than two years of experience with `scala` (and, frankly, only about three years of coding in any language).


##Main Advancements

I will highlight some of the main improvements that I was able to make to this system.  
**Note:** While I have many ideas of how to improve the *performance* of the classifier itself, most of my improvement were more related to the engineering of the system than the features themselves, with one exception that is explained below.
**Note:** I will not go into much detail here about the features used, but I'm more than happy to discuss that in more detail, along with the multitude of further improvements I have in mind for this system.


###Added Feature

I actually only implemented one feature to this "new-and-improved" code that didn't exist in the previous version: sentence coherence.  I had built this in my work for PitchVantage because we wanted a metric to evaluate how clearly our users were transitioning from one sentence to another.  And while the `processors` system that I've used for the NLP pipeline *does* have coreference resolution, we decided against using it for two reasons: (1) it's computationally expensive and (2) it doesn't work all that well.

So I essentially built a system to approximate coreference, but done more from a rule-based perspective.  I'll explain it briefly below:

In most cases when writing or speaking, you want to make sure that each sentence is related to the sentence that came before, and this is most easily measured by looking at how the similarity of nouns between sentences.  This metric examines pairs of sentences and looks for references in the *second* sentence to nouns that appeared in the *first* sentence.  For each "reference", the coherence score is increased by 1.  

I designed three different types of reference:

1. The same word is referenced in both sentence #1 and sentences #2.
2. A nominative pronoun appears in the subject position of the main clause of sentence #2.  The naive assumption made is that this pronoun is referring to something in sentence #1.
3. A word appears in sentence #2 that is "similar" to a word in sentence #1.  Similarity here is measured as the cosine similarity score of the two word vectors is above a certain threshold (admittedly, I have not found the optimal threshold as I continue to find instances where I think two words *should* be considered semantically similar but their cosine similarity score is low while other words that I think have no semantic link has high cosine similarity scores).  See below for an example of each of these.

![alt tag](https://raw.githubusercontent.com/michaelcapizzi/TextComplexity/master/src/main/resources/Coherence%20visual.JPG)


This feature comes with some high costs; however.  The word vectors I use are trained on all of Gigaword, and the compressed version of that file is still almost 1G in size.  I've pruned it with what I hope is minimal impact on robustness, but it's still a large file.  And importing that file into memory takes time as well.  See [Efficiency](#efficiency)


###Clarity

I use the Intellij IDE for `scala`, and the code completion is a great asset.  It does, however allow me to generate relatively incomprehensible lines of code, and an example of this was my over-reliance on `map`.  I think I used to believe that use of `map` was more "elegant" than a standard `for` loop, and so I would use it all the time.  For example:

```scala
this.document.map(_.sentences.map(_.words.toVector)).flatten.flatten
```

And I suppose if "elegant" is defined as "takes up only one line", then this would qualify.  But I've come to appreciate the importance of comprehensible code, and so I have replaced most instances (I still think, obviously, that `map` has its place) of iterations with a standard `for` loop for clarity.

```scala
(for (sentence <- this.document) yield {
    for (word <- sentence) yield {
        word
    }
}).flatten.flatten
```

And just yesterday, my friend showed me that when you are using nested loops (which, when dealing with documents, paragraphs, sentences, and words, happens frequently in this code), you can put all the `for` statements together:

```scala
(for (
        sentence <- this.document;
        word <- sentence
    ) yield {
        word
}).flatten.flatten
```

But just as there are many times and places where a simple structure like a `for` loop are probably best, there are times when other structures can make code even cleaner.  When I wrote this original code I  relied too heavily on `if` and `then` statements:

```scala
val regex = if (POS == "noun") {
                "NN.?"
            } else if (POS == "adjective") {
                "JJ.?"
            } else if (POS == "verb") {
                "VB.?"
            } else if (POS == "adverb") {
                "RB.?"
            } else {
                "NN.?"
            }
```

It's just too verbose, and for no good reason.  Furthermore, the use of `if` and `then` cause problems because variables instantiated as an immutable with `val` are *only* available inside those conditions.  But now that I have a better understanding of the immutable variable, `var`, and case matching in `scala`, I can rewrite the above in a much cleaner way:

```scala
var regex = ""

    POS match {
      case "noun" => regex = "NN.?"
      case "adjective" => regex = "JJ.?"
      case "verb" => regex = "VB.?"
      case "adverb" => regex = "RB.?"
      case _ => regex = "NN.?"
    }
```


###Documentation

I have always added comments to my code, but it was never particularly systematic, and it lacked any portability; You had to be "knee-deep" into the code before you could even see the comments.  And then I discovered [`scaladoc`](http://docs.scala-lang.org/style/scaladoc.html).  It was a simple shift to simply utilized `scaladoc` syntax in my comments, but the bigger shift is in the mentality of documentation for the sake of others.  I now write my comments in the `scaladoc` syntax and in such a way as to facilitate others' understanding of my code.

A friend of mine showed me some plugins for publshing the documen generated by `scaladoc` on a web page, but I haven't committed time to figuring that out yet.  In the meantime, a few screenshot of the documentation are [here](https://raw.githubusercontent.com/michaelcapizzi/TextComplexity/master/src/main/resources/scaladocScreenShots/scaladoc-1.png) and [here](https://raw.githubusercontent.com/michaelcapizzi/TextComplexity/master/src/main/resources/scaladocScreenShots/scaladoc-2.png). 


###Unit Testing

I had always "tested" my code at every step, essentially making sure that (1) it compiles, (2) it "gets all the way to the end" and (3) the output is the right one.  The only problem was that I never kept those tests around nor did I document how I tested.  And so that same friend of mine (from whom I've learned a lot about coding) said to me, "Then you're not really testing.  Because it's the reproducibility that is the most important aspect of testing."  He introduced me to unit testing in `scala`.  You can see some of the tests [here](https://github.com/michaelcapizzi/TextComplexity/blob/master/src/test/scala/Tests.scala).  

I should note here that I still struggle with knowing how to write good tests.  I think that "test coverage" is an important aspect of programming, but I haven't figured out how best to navigate the balance between "coverage" and "writing a test for everything."  I'm not confident that the tests I've begun to write are effective enough, but I imagine that, like anything else, seeing good examples in practice can help me to more clearly understand the nuances of unit testing.


###Efficiency

When I wrote this code the first time, I had a deadline (when the homework was due), and even though I started earlier than most of my classmates, I felt rushed.  As a result, I just wanted to get things to work, efficient or not, and most of the code ended up in the "not efficient" class.  Since I've written this code, I've become more aware of how efficient run-time can often times become the most important factor in writing code.  I've timed out of numerous coding challenges, not because my code didn't work, but because it simply wasn't efficient.  And having worked with language data long enough now, I've learned that multiple passes through a document simply is not practical.  Below is an example where I was able to iterate through the tokens of the document only one time and gather all the information (in this case, simple frequency counts of tokens, lemmas, tags, and entities) that I needed.  And when analyzing _Don Quixote_, this is incredibly important.

```scala
val tokenCounter = new Counter[String]()
    val lemmaCounter = new Counter[String]()
    val tagCounter = new Counter[String]()
    val properNounCounter = new Counter[String]()
    val tokenTagCounter = new Counter[(String, String)]()
    val lemmaTagCounter = new Counter[(String, String)]()

    for (word <- this.lexicalTuple(withPunctuation = false).flatten) {
      tokenCounter.incrementCount(word._1.toLowerCase)
      lemmaCounter.incrementCount(word._2._1.toLowerCase)
      tagCounter.incrementCount(word._2._2)
      tokenTagCounter.incrementCount(word._1.toLowerCase -> word._2._2)
      lemmaTagCounter.incrementCount(word._2._1.toLowerCase -> word._2._2)
      if (word._2._3 == "PERSON" || word._2._3 == "LOCATION") {
        properNounCounter.incrementCount(word._1)
      }
    }
```


Another place where efficiency comes into play is the use of word embeddings.  The file containing these embeddings is huge, and finding efficient ways to prune the search space is vital.  Below is an explanation of how I designed the `Word2Vec` class in this system.  **Note**: I am *not* talking about training vectors here; I use pretrained ones.

Loading 1.5 million word vectors into memory is slow and unnecessary.  Luckily, the pretrained vectors I have are sorted by frequency, and some random spot-checking of the word vectors at different indices, I determined that I can "get away with" taking the first 500k vectors.  But even then, if I'm trying to implement a method like `findClosestWords`, this would require *n* (size of the vocabulary) number of calculations of cosine similarity and *then* a sorting.  

So my simple approach was to use k-means clustering to group the vectors (If the vocabulary for a given document is above a certain threshold.  Right now I have that threshold set at 1000 words) into 10 clusters.  So now if I want to find the closest words to "cat", I first calculate the cosine similarity of "cat" to the 10 centroids, and then only have to calculate the cosine similarity to all of the words in that given cluster, essentially cutting my computations by a factor of 10.
  
**Note**: While in theory this is efficient, I must admit that as I am writing this, my system has been running for nearly 24 hours (in parallel) to try to cluster the word vectors for the vocabularies of all 54 documents in my dataset.  I'd obviously rather have computational costs like this on the training side of the process, but I can't help but wonder a few things:

1. Is the inclusion of word embeddings to my coherence metric (explained [here](#added_feature)) really that important?  How much would my classification accuracy decrease if I only use the other two means of measuring coherence, considering this is only one of many syntactic features?
1. Would the computational costs of calculating cosine similarity of all the words in the vocabulary (when there are no clusters) be that much higher than the time it takes to cluster a document in the first place?

The answers to these questions require more testing, but I think, in the long run, word embeddings will provide the opportunity for many more informative features (how would a neural network or LSTM language model that uses word embeddings as input perform?  Either as a stand alone classifier or as an added feature?), and so it's worth the computational cost that comes with them.


###Overall System Design

In my first attempt at this code, I decided to treat each document as a list of paragraphs.  This was done with good intentions: I wanted to gather features surrounding the types of discourse markers found by the Discourse Parser, but that is computationally expensive as it generates *one* discourse parse for the entire document.  This would be impractical when dealing with _Don Quixote_.  But in handling each paragraph separately, I now have relatively incomprehensible data structures at each instance.  For example, the data structure used for capturing tokens was a `Vector[Vector[Vector[String]]]`.

And so I spent a long time thinking about what the best way to structure this updated system would be.  In the end, I still stuck with segmenting at the paragraph level (for the reason explained above), but a simple modfication was to create a class (`ProcessedParagraph`) for each paragraph, and then make the document class (`TextDocument`) as a collection of `ProcessedParagraphs`.  
 
 I still don't know if this ideal, as the resulting data structures in the `TextDocument` class are still unwieldly at times, but at least this time I can, if I want to, inspect each paragraph as an individual unit.  Since I have a very small dataset from which to train on (I can only use documents or excerpts for which National Reading Standard Grade Levels are published, which to my knowledge is still a very small subset of texts), I considered the idea of treating each paragraph as its own training instance, and in this case, having the unit of measure at the paragraph level would be necessary.  But I decided against it because I think it would inject entirely too much noise into my already-noisy data; it's fallacious to assume that every single paragraph in _Fahrenheit 451_, for example, exhibits characteristics of 9th-10th grade writing.
 
 In general, however, I've learned (mainly by looking at the code written by my old advisor and his research assistants) how to structure things in a logical and modular way.  For this system, each type of feature has its own class and I use objects to house methods that all have something in common (they usually are all methods used to support a particular class).





