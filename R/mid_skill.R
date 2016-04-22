# Author: Ryan Brady

# if not using Rstudio project:
# setwd("..")

attach(onet)

education.reqs <- subset(`Education, Training, and Experience`, Scale.ID == 'RL') %>% 
  group_by(O.NET.SOC.Code, Element.Name) %>% 
  dplyr::summarize(value = weighted.mean(Category, w = Data.Value))

education.reqs$mid.skill <- education.reqs$value >=2 & education.reqs$value <=6
education.reqs$Element.Name <- NULL
saveRDS(education.reqs, "data/processed/mid_skill.RDS")
