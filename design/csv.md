Adding CSV/TSV fns, tired of constantly redoing these.

# Basic
- read tsv/csv,
- various other options
- from files or resources or urls
- raw form returns array of fields
- ms form returns mapseq, assuming header row for columns

# Rationale
I keep rewriting and copying versions of these, it's insane

# Plan

- TODO gather features from all various implementations (voracious,. traverse, probably others)
- write the perfect synthesis in cljcore.clj (because I want these ready-to hand.) Would put in data but that is .cljc so would be a pain (or require conditionals or something) WAIT actually I want them in .cljs of course, but the sources will be different. 

OK put in data.cljc with appropriate conditionals. Does .cljs even have access to a cvs library?

Alright figure this out

Another note: way allows front-end access to back-end resources, so having a read-csv in cljs is not actually needed, more trouble than it is worth

# Extras

## TODO Should create the directory path if necessary. 

That's almost always what I want rather than having to explicitly fart with directory creation.


## TODO every fn should take an optional :columns arg



