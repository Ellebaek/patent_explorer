# patent_explorer
Hacked at Novozymes Bibliometrics & Text-mining Hackathon 7-8 October 2019. Simple shiny app to load and explore patent database extract from Questel Orbit (https://www.orbit.com/). Only the first four fields of the example file is used in the current version of the app.

Download and run from the terminal with following command
```
R -e "shiny::runApp('.../patent_app')"
```
replacing `...` with the full path to where you download the app. You might also need to update `readLines("./patent_data_raw.csv")` with full path reference to the downloaded example file.

Or try it live with example data at https://ellebaek.shinyapps.io/patent_app/

## Using your own data extract
If you export your own patent search from Questel Orbit, then update `readLines("./patent_data_raw.csv")` to point to your own file and in case (most likely) the column order do not match the example file, also update `col_ids <- c(1,2,3,4)` to map the columns of your source file correctly to the app.

And you probably want to update the hardcoded data cleaning of the assignee list, which is taking place in the `clean_assignee` function.
