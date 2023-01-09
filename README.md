# Semantic verification for Abstract Wikipedia

This is a project about semantic verification of a Grammatical Framework grammar for expressing facts about countries and capitals in English and Swedish, using Haskell, Montague semantics and WikiData.

A much more detailed account of this project is provided [in the report](https://odr.chalmers.se/items/691228a5-462e-4539-ba03-adba603d6ac9).

## Grammar

The grammar for this project is the same as the [country-facts-grammar](https://github.com/danieelst/country-facts-grammar). The PGF-file of this grammar can be found in `/data`.

Some example texts can be found in `/example-texts`.

## Example

Consider the statement

`the population of Sweden is about 10 million .`

This text is well-formed according to the Grammatical Framework grammar and will be parsed by the program as the following abstract syntax tree

`OneSentenceDoc (FactSentence (AttributeFact PopulationAttribute (NameObject (MkName Sweden_CName)) (NumericValue (AboutNumeric (IntMillionNumeric 10)))))`

In turn, this tree will be interpreted as the following first-order logic formula

`population(Sweden) ≈ 10000000`

Which can be verified against the data model.

## WikiData

To fetch new data from WikiData, simply run the Python program `query.py` in the directory `/wikidata`. Note that the working directory must be that directory, since it writes the new data to the relative path of the `/data` directory.

## Commands

To build the project, use the command:

`stack build`

To run the application, use the command:

`stack exec main`

To perform a sanity check, use the command:

`stack exec test`
