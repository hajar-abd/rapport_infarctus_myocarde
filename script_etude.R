################################################################
#
# Sujet d'étude --> Interactions Complexes entre BPCO et Infarctus du Myocarde : Impact des Antécédents Pulmonaires sur les Complications Cardiaques  
#
# Hâjar ABDAOUI 
# Aisha HELLI 
# Karima ZOUZANE
#
# 25/12/2023
#
################################################################

# Packages installation 
################################################################
library(dplyr)
library(ggplot2)

# Read the files
################################################################
myocard = read.csv("data/myocard.csv")
str(myocard)

# Data Management
################################################################
etude_myocard <- myocard %>%
  select(ID, AGE, SEX, 
         INF_ANAM,
         zab_leg_01, zab_leg_02, zab_leg_03, zab_leg_04, zab_leg_06,
         FIBR_PREDS, PREDS_TAH, JELUD_TAH, FIBR_JELUD, RAZRIV, DRESSLER, ZSN, REC_IM) %>%
  rename("id" = ID, "age" = AGE, "sex" = SEX, 
         "infarctus_myocard" = INF_ANAM, 
         "bronchite" = zab_leg_01, "bronchite_obs" = zab_leg_02, "asthme" = zab_leg_03, "pneumonie" = zab_leg_04, "tuberculose" = zab_leg_06,
         "fibrillation_a" = FIBR_PREDS, "tachycardie_s" = PREDS_TAH,"tachycardie_v" = JELUD_TAH,"fibrillation_v" = FIBR_JELUD, 
         "rupture_myocarde" = RAZRIV, "syndrome_d" = DRESSLER, "insuffisance_c" = ZSN, "rechute_im" = REC_IM) %>%
  distinct() %>%
  na.omit()

etude_myocard$sex <- factor(etude_myocard$sex, levels = c(0,1), labels = c("HOMME","FEMME"))
variables <- c("bronchite", "bronchite_obs", "asthme", "pneumonie", "tuberculose", 
               "fibrillation_a", "tachycardie_s", "tachycardie_v", "fibrillation_v", "rupture_myocarde", 
               "syndrome_d", "insuffisance_c", "rechute_im")
for (var in variables)
  etude_myocard[[var]] <- factor(etude_myocard[[var]], levels = c(0, 1), labels = c("NON", "OUI"))
etude_myocard$infarctus_myocard <- factor(etude_myocard$infarctus_myocard,
                                          levels = c(0,1,2,3), 
                                          labels = c("ZERO","UN","DEUX","TROIS ET PLUS"))

### d) Statistiques descriptives --> ajouter dans script final
etude_myocard$infarctus <- ifelse(etude_myocard$infarctus_myocard != "zero", 1, 0 )
etude_myocard$infarctus <- factor(etude_myocard$infarctus, levels = c(1,0), labels = c("OUI", "NON"))
table(etude_myocard$infarctus)

summary(etude_myocard) # Coup d'œil sur notre base de données

# Illustrations graphiques
################################################################

# a) Répartition Hommes Femmes 
repartition <- etude_myocard %>%
  ggplot(aes(x = sex, fill = sex, label = after_stat(count))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#007afd", "#FD6C9E")) +
  labs(x = "", y = "Effectif", fill = "Genre", 
       title = " Graphique 1 : Répartition des patients par genre") + 
  theme_bw() + 
  geom_text(stat = "count", vjust = -1, col = "black", fontface = "bold") +
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(colour = "black"),
        strip.background = element_rect(colour = "black", fill = "white"), 
        legend.position = "none") +
  scale_y_continuous(limits = c(0,600))
print(repartition)
ggsave("repartition_homme_femme.png", repartition, width = 6, height = 4, dpi = 300)

# b) Distribution âge --> dans le rapport et diapo
histogramme <- etude_myocard %>%
  ggplot(aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 1, color = "#e9ecef", alpha = 0.5, position = "identity") + 
  scale_fill_manual(values = c("#007afd", "#FD6C9E")) +
  labs(x = "Âge (en années)", y = "Effectif", fill = "Genre", 
       title = " Graphique 2 : Âge des patients en fonction du genre") + 
  theme_bw()
print(histogramme)
ggsave("histogramme.png", histogramme, width = 6, height = 4, dpi = 300)

# Boxplot âge en fonction du sexe
etude_myocard %>%
  group_by(sex) %>%
  ggplot(aes(sex, age, fill = factor(sex))) +
  geom_boxplot(color = "black") + 
  labs(x = "Sexe du patient", y = "Âge lors de l'admission", 
       title ="Âge des patients en fonction du genre", fill = "Genre") + 
  theme_bw() + 
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c("#007afd", "#FD6C9E"))
  
# c) Quantité d'infarctus
quantite_infarctus <- etude_myocard %>%
  ggplot(aes(x = infarctus_myocard, fill = factor(infarctus_myocard))) +
  geom_bar(color = "black", position = "dodge") +
  labs(x = "Quantité d'infarctus du myocarde", y = "Fréquence", 
       title = "Graphique 3 : Nombre de patients par quantité d'infarctus du myocarde", 
       fill = "Nombre d'infarctus") +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black")) +
  scale_fill_manual(values = c("#2B3B5D","#0E7DC8", "#439ED7", "#E5F2F9"))
print(quantite_infarctus)
ggsave("quantite_infarctus.png", quantite_infarctus)


# Exploration des variables - Ajustement de modeles de regression logistique
##############################################################################
# Etudier la dépendance entre les antécédents pulmonaires et les possibles complications cardiaques suite à un infarctus du myocarde. 
# Nos variables à expliquer sont les facteurs de complications cardiaques de l'infarctus du myocarde. 
# Nos variables explicatives sont les antécédents pulmonaires et plus particulièrement la BPCO.


#### a) Fibrillation auriculaire
reg_fib_a <- glm(fibrillation_a ~ bronchite + bronchite_obs + asthme + pneumonie + tuberculose, 
               data = etude_myocard, family = binomial)
summary(reg_fib_a)
## Les p-valeur de bronchite_obs et asthme sont inférieur à 5% 
## Donc la survenue de fibrillation auriculaire dépend de la bronchite obstructive et de l'asthme. 

## Calcul du odds-ratio (OR) : 

exp(0.58952) #On obtient pour bronchite_obs un OR = 1.803123 
###Toute chose egal par ailleurs (TCEPA), une personne ayant une bronchite chronique obstructive dans l'anamnèse aura 80% de chance en plus qu'une autre personne d'avoir une fibrillation auriculaire. 

exp(1.29562) #On obtient pour asthme un OR = 3.65326
###TCEPA, une personne ayant de l'asthme aura 265% de chance en plus qu'une autre personne d'avoir une fibrillation auriculaire. 

#### b) Tachycardie supraventriculaire
reg_tac_s <- glm(tachycardie_s ~ bronchite + bronchite_obs + asthme + pneumonie + tuberculose,
                 data = etude_myocard, family = binomial)
summary(reg_tac_s) 
## Toutes les p-valeur sont supérieures à 5%.
## Soit, la survenue d'une tachycardie supraventriculaire suite à un infarctus du myocarde chez une personne ne sera pas liée à ces antécédents de complications pulmonaires. 

#### c) Tachycardie ventriculaire
reg_tac_v <- glm(tachycardie_v ~ bronchite + bronchite_obs + asthme + pneumonie + tuberculose, 
                 data = etude_myocard, family = binomial)
summary(reg_tac_v) 
## La p-valeur de bronchite_obs est inférieur à 5%. 
## Donc la survenue de tachycardie ventriculaire dépend de la bronchite chronique obstructive. 

## Calcul du odds-ration (OR) : 
exp(1.3419) #On obtients pour bronchite_obs un OR = 3.826307 
###TCEPA, une personne ayant une bronchite chronique obstructive aura 283% de chance en plus d'avoir une tachycardie ventriculaire. 

#### d) Fibrillation ventriculaire
reg_fib_v <- glm(fibrillation_v ~ bronchite + bronchite_obs + asthme + pneumonie + tuberculose, 
                 data = etude_myocard, family = binomial)
summary(reg_fib_v) 
## La p-valeur de bronchite_obs est inférieur à 5%. 
## Donc la survenue d'une fibrillation ventriculaire dépend de la bronchite chronique obstructive

## Calcul du odds-ration (OR) : 
exp(0.9223) #On obtients pour bronchite_obs un OR = 2.515068
###TCEPA, une personne ayant une bronchite chronique obstructive dans l'anamnese aura 152% de chance en plus d'avoir une fibrillation ventriculaire. 

#### e) Rupture du myocarde
reg_rup_m <- glm(rupture_myocarde ~ bronchite + bronchite_obs + asthme + pneumonie + tuberculose, 
                 data = etude_myocard, family = binomial)
summary(reg_rup_m) 
## Toutes les p-valeur sont supérieures à 5%.
## Soit, les antécédents de complications pulmonaires n'auraient donc aucun effet sur une potentielle rupture du myocarde 

#### f) Syndrome de Dressler
reg_dre_s <- glm(syndrome_d ~ bronchite + bronchite_obs + asthme + pneumonie + tuberculose, 
                 data = etude_myocard, family = binomial)
summary(reg_dre_s) 
## Toutes les p-valeur sont supérieures à 5%.
## Soit, les antécedents de complications pulmonaires ne seraient pas à l'origine du syndrome de DRESSLER (Syndrome post-infarctus = forme de péricardite tardive )

#### g) Insuffisance cardiaque
reg_ins_c <- glm(insuffisance_c ~ bronchite + bronchite_obs + asthme + pneumonie + tuberculose, 
                 data = etude_myocard, family = binomial)
summary(reg_ins_c) 
## La p-valeur de bronchite est inférieur à 5%. 
## Donc une Insuffisance cardiaque chronique suite à l'infarctus dépend d'une bronchite. 

## Calcul du odds-ration (OR) : 
exp(0.9188) # On obtient pour bronchite un OR = 2.506281
### TCEPA, une personne ayant une bronchite aura 151% de chance en plus d'avoir une insuffisance cardiaque chronique.

#### h) Rechute d'infarctus du myocarde
reg_re_im <- glm(rechute_im ~ bronchite + bronchite_obs + asthme + pneumonie + tuberculose, 
                 data = etude_myocard, family = binomial)
summary(reg_re_im) 
## Les p-valeur de bronchite_obs et bronchite sont inférieures à 5%. 
## La rechute de Donc une bronchite chronique obstructive et celle sans anamnese dependent bien d'une rechute de l'infarctus du myocarde. 

## Calcul du odds-ration (OR) : 
exp(0.58458) # On obtient pour la variable bronchite_obs un OR = 1.794237 
### TCEPA, une personne ayant une bronchite chronique obstructive aura 79% de chance en plus qu'une autre personne d'avoir une rechute de l'infarctus du myocarde 
exp(-0.98293) # On obtient pour la variable bronchite un OR = 0.374213
### TCEPA, une personne ayant une bronchite  aura 62% de chance en moins qu'une autre personne d'avoir une rechute de l'infarctus du myocarde. 

#### i) Bronchite Pulmonaire Chronique Obstructive (BPCO) et les complications cardiaques

regbronchite <- glm(bronchite_obs ~ fibrillation_a + tachycardie_s + tachycardie_v + fibrillation_v + rupture_myocarde + syndrome_d + insuffisance_c, 
                    data = etude_myocard, family = binomial)
summary(regbronchite)
## On obtient exactement les résultats obtenus avec les modèles de régressions précédents :
## C'est à dire qu'une personne ayant une BPCO et ayant subi un IM a de fortes chances d'avoir les complications cardiaques suivantes : fibrillation auriculaire, tachycardie ventriculaire, fibrillation ventriculaire et une probable rechute de l'infarctus du myocarde.
## Mais la BPCO n'aura aucune implications/ ne déclenchera pas de tachycardie supraventriculaire, de rupture du myocarde ou de syndrome de DRESSLER 

exp(0.47406) 
# TCEPA, une personne présentant une bronchite chronique obstructive aura une probabilite d’avoir une fibrillation auriculaire 
#comme complication cardiaque suite à un infarctus du myocarde qui est multiplié par e0.47406 = 1.606503 
#par rapport à une personne n'ayant pas eu cet antécédent pulmonaire.

exp(1.31383)
# TCEPA, une personne présentant une bronchite chronique obstructive aura une probabilite d’avoir de la tachycardie ventriculaire 
#comme complication cardiaque suite à un infarctus du myocarde qui est multiplié par e1.31383 = 3.720396 
#par rapport à une personne n'ayant pas eu cet antécédent pulmonaire.

exp(0.81476)
# TCEPA, une personne présentant une bronchite chronique obstructive aura une probabilite d’avoir une fibrillation ventriculaire 
#comme complication cardiaque suite à un infarctus du myocarde qui est multiplié par e0.81476 = 2.258634 
#par rapport à une personne n'ayant pas eu cet antécédent pulmonaire.

# Supposons H0 et H1 tel que : 
# Hypothèse nulle (H0) : La fibrillation et la tachycardie ventriculaires sont indépendantes l'une de l'autre.
# Hypothèse alternative (H1) : Il existe une dépendance significative entre la fibrillation et la tachycardie ventriculaires.

chisq.test(etude_myocard$fibrillation_v, etude_myocard$tachycardie_v)

# Nous obtenons une p-valeur = 4.455e-06 soit inférieur à 5% 
# Ainsi nous rejetons H0 au profit de H1 soit la tachycardie ventriculaire entraîne  de la fibrillation ventriculaire. 

#### CONCLUSION PARTIE 2 :  !!!!!!!!!!
# les facteurs de complications pulmonaires tels que BPCO, bronchite, asthme sont impliqués dans la survenue de complications cardiaques suite à un infarctus du myocarde
# les patients atteints de BPCO sont sujets à plus de complications cardiaques que les patients sains. 
# importance de la prévention +++








  
  
  
  
  
  