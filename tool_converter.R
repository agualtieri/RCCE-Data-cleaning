## VENA Assessment - Tool converter
## alberto.gualtieri@reach-initiative.org 
## V1
## 28/08/2020

rm(list=ls())

## Download package from repo
# remotes::install_github("hedibmustapha/QuestionnaireHTML",build_opts = c())

library("QuestionnaireHTML")


## Run function
questionnaire_to_html(survey.file = "./input/kobo/questions.csv", choices.file = "./input/kobo/choices.csv", choices.label = "label", survey.label = "label", 
                      output.dir = "./output/", output.filename = "rcce_questionnaire.html", title = "", right.to.left = FALSE, special.characters ="")
