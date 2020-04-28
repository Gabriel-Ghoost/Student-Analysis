
#Correspondence analysis

PF <-data.frame(world.region, unaivailable.program,low.educational.quality
                ,competitive.admission, advantage.of.international.degree
                ,unavailability.of.scholarship ,encouragement.from.family,encouragement.from.friends
                ,better.earning.prospects, social.prestige, experience.different.culture)






library("FactoMineR")
library("factoextra")

dim(PF)
colnames(PF)
colnames(ca.active)
poison.active <- poison[1:417, 6:15]


ca.active<-PF[4, 6:15]

head(ca.active[, 1:6], 3)


res.mca <- MCA(ca.active, graph = FALSE)

print(res.mca)


eig.val <- get_eigenvalue(res.mca)

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))


fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

var <- get_mca_var(res.mca)
var

fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())



fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())




head(PF)

cats <- apply(PF, 2, function(x) nlevels(as.factor(x)))

cats
#MCA

mca1 = MCA(PF, graph = FALSE)

# list of results
mca1

#table of eigenvalues
mca1$eig


mca1_vars_df <- data.frame(mca1$var$coord, Variable = rep(names(cats), cats))


mca1_obs_df <- data.frame(mca1$ind$coord)

ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour="rainbow")) +
  ggtitle("MCA plot of variables using R package FactoMineR")




install.packages("MASS")
library(MASS)

mca2 <- mca(newtea, nf = 5)


PF<-PullFactors[,c("world.region", "employment_prospect", "geo.cul.proximity", "living.cost", "available.scholarship",
                   "HSE_quality", "HSE_reputation", "program_choice", "russian.culture", "recognition.of.HSE.degree",
                   "family.friends.recommendations", "HSE.rank", "international.students.support", "HSE.alliances")]



poison.active <- PF[1:417, 2:13]

head(poison.active[, 1:6], 3)


for (i in 1:4) {
  plot(poison.active[,i], main=colnames(poison.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}


res.mca <- MCA(poison.active, graph = FALSE)

print(res.mca)

eig.val <- get_eigenvalue(res.mca)
head(eig.val)

fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))


fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())



var <- get_mca_var(res.mca)
var


head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)


fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


head(round(var$coord, 2), 4)



fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


fviz_mca_var(res.mca, col.var="black", shape.var = 15,
             repel = TRUE)



head(var$cos2, 4)



fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())



fviz_mca_var(res.mca, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())



library("corrplot")
corrplot(var$cos2, is.corr=FALSE)


fviz_cos2(res.mca, choice = "var", axes = 1:2)


head(round(var$contrib,2), 4)


# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)


fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)


fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())


ind <- get_mca_ind(res.mca)
ind


# Coordinates of column points
head(ind$coord)
# Quality of representation
head(ind$cos2)
# Contributions
head(ind$contrib)


fviz_mca_ind(res.mca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping (slow if many points)
             ggtheme = theme_minimal())


# Cos2 of individuals
fviz_cos2(res.mca, choice = "ind", axes = 1:2, top = 20)
# Contribution of individuals to the dimensions
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)


fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "world.region", # color by groups 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal())



res.mca <- MCA(PF, quali.sup = 1,  graph=FALSE)

# Supplementary qualitative variable categories
res.mca$quali.sup


fviz_mca_biplot(res.mca, repel = TRUE,
                ggtheme = theme_minimal())



fviz_mca_var(res.mca, choice = "mca.cor",
             repel = TRUE)



