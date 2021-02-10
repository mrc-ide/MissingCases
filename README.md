# MissingCases
Contact tracing, where exposed individuals are followed up to break ongoing transmission chains, is a key pillar of outbreak response for many infectious disease outbreaks, such as Ebola and SARS-CoV-2. Unfortunately, these systems are not fully effective, and cases can still go undetected as people may not know or remember all of their contacts or contacts may not be able to be traced. A large proportion of undetected cases suggests poor contact tracing and surveillance systems, which could be a potential area of improvement for a disease response. 

This package enables users to calculate the proportion of cases that are not reported from case line list and contact tracing data. It uses the method from Unwin et al. (2021), which uses a next generation approach.


## Installation
This pacakge can be installed from github using devtools:
install.pacakages("devtools")
install_github("mrc-ide/MissingCases")

## Examples
Two examples are provided in the vignettes:
1) SARS-CoV-2 in New Zealand during 2020
2) Guinea Ebola outbreak in 2014.
