library(reshape2)
library(lattice)
library(corrplot)

attach(onet)

skills.wide <- dcast(subset(Skills, Scale.ID == "LV"), 
                     O.NET.SOC.Code ~ Element.Name, 
                     value.var = "Data.Value")
abilities.wide <- dcast(subset(Abilities, Scale.ID == "LV"), 
                        O.NET.SOC.Code ~ Scale.ID + Element.Name, 
                        value.var = "Data.Value")
knowledge.wide <- dcast(subset(Knowledge, Scale.ID == "LV"), 
                        O.NET.SOC.Code ~ Scale.ID + Element.Name, 
                        value.var = "Data.Value")

education.wide <- dcast(`Education, Training, and Experience` , O.NET.SOC.Code ~ Scale.ID + Element.Name + Category, value.var = "Data.Value")

plot(princomp(skills.wide[,-1]))
plot(princomp(abilities.wide[,-1]))
plot(princomp(knowledge.wide[,-1]))

png("plots/skills_cor.png")
  corrplot(cor(t(as.matrix(skills.wide[,-1]))), order = "FPC", tl.pos = 'n', method = 'color')
dev.off()
png("plots/abiliities_cor.png")
  corrplot(cor(as.matrix(abilities.wide[,-1])), order = "FPC", tl.pos = 'n', method = 'color')
dev.off()
png("plots/knowledge_cor.png")
  corrplot(cor(as.matrix(knowledge.wide[,-1])), order = "FPC", tl.pos = 'n', method = 'color')
dev.off()
