# D_labialis

This repository contains the supplementary materials of Ballen (2018) on variation of mouthparts in _Dendropsophus labialis_ tadpoles.

## What's in this repository?

### Datasets

Three comma-separated files (`*.csv`) files containing: 

- The types of anomalies and their frequency per lot examined (`anomalies.csv`)

- The data associated to pre-larval ontogeny (`embryos.csv`), and

- The data associated to larval ontogeny (`larvae.csv`)

### Scripts

The statistical results of the study can be reproduced bu running the script `ontogeny.R` that call the function placed in `iter.cor.test.R`. The output of the script are the Tables 1 and 2, and the Figure 1 that stacks together three plots. The latter image is saved in `png` format.

## Dependencies

The script rely on the `xlsx` package so you will receive an error if try to run the script without installing such dependency. 

## How to use it

Since both scripts and data are in the same directory (i.e., this repository does not have subdirectories), you might reproduce the analysis running

```{R}
source("ontogeny.R")
```
 in an R interactive session. Please keep in mind that some output might be prompted to the screen without writing to a file taking place (e.g., when calculating the Gosner stage intervals for each lot).

# References

Ballen, G.A. 2018. Tooth row variation in tadpoles of *Dendropsophus labialis* (Anura: Hylidae: Dendropsophini) and the evolution of oral morphology in the genus. Caldasia 40(2):216-231.
