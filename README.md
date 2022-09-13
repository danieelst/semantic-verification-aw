# Semantic verification for Abstract Wikipedia

This is a project about semantic verification of a Grammatical Framework grammar for expressing facts about countries and capitals in English and Swedish, using Haskell, Montague semantics and WikiData.

## Grammar

The grammar for this project is the same as the [country-facts-grammar](https://github.com/danieelst/country-facts-grammar). The PGF-file of this grammar can be found in `/data`.

Some example texts can be found in `/example-texts`.

## WikiData

To fetch new data from WikiData, simply run the Python program `query.py` in the directory `/wikidata`. Note that the working directory must be that directory, since it writes the new data to the relative path of the `/data` directory.

## Commands

To build the project, use the command:

`stack build`

To run the application, use the command:

`stack exec main`

To perform a sanity check, use the command:

`stack exec test`
