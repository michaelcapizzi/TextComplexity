#Requirements

- Java 8
- [sbt](http://www.scala-sbt.org/download.html)
- [Gitub Large File Storage (lfs)](https://git-lfs.github.com/)

#Installation

##Install Github LFS

This is required for the large model files that are dependencies for the system.

1. Download and run `./install.sh`
2. `git lfs install`

##Clone the repository
```
git clone https://github.com/michaelcapizzi/TextComplexity.git
```


#How to Run

The system expects the text to be classified to be in `.txt` form with paragraphs delimited by a blank line:

```
She moved away from the door, stepping as softly as if she were afraid of awakening some one.  She was glad that there 
was grass under her feet and that her steps made no sounds.  She walked under one of the fairy-like gray arches between 
the trees and looked up at the sprays and tendrils which formed them.  "I wonder if they are all quite dead," she said.  
"Is it all a quite dead garden? I wish it wasn't."

If she had been Ben Weatherstaff she could have told whether the wood was alive by looking at it, but she could only 
see that there were only gray or brown sprays and branches and none showed any signs of even a tiny leaf-bud anywhere.
```

**Note:**You *can* import a `.txt` file where the paragraphs are *not* delimited by a blank line, but computational times may
explode as the feature extractor depends upon a discourse parse which handles the entire document as one instance 
to parse.


The system can handle two different classification structures:

Classify into 6 distinct classes:

K-1 | 2-3 | 4-5 | 6-8 | 9-10 | 11-12
---|---|---|---|---|---
"0001" | "0203" | "0405" | "0606" | "0910" | "1112"

or 

Classify into 3 distinct classes:

 K-5 | 6-8 | 9-12
 ---|---|---
 "0005 | "0608" | "0912"


If you'd like to simply run the best-performing model for each structure, you can run this command: `run-main Complexity.Demo [file to analyze] [number of classes]`

```
run-main Complexity.Demo "document.txt" "3"
```

or 

```
run-main Complexity.Demo "document.txt" "6"
```


You will see the feature values generated, the predicted grade level band, and the confidence of the other classes for comparison.




