# Antécédents pulmonaires et complications après un infarctus

Projet universitaire d'analyse de données cliniques avec r. L'étude explore les associations entre des antécédents pulmonaires, notamment la bronchite chronique obstructive, et différentes complications observées après un infarctus du myocarde.

Le dépôt contient les données utilisées, les analyses, un document r markdown, le rapport final et le support de présentation.

## Démarche d'analyse

- Importation du fichier `data/myocard.csv`.
- Sélection et renommage des variables, suppression des doublons sur les variables sélectionnées et des lignes incomplètes.
- Recodage des variables qualitatives et description de la population à l'aide de graphiques.
- Ajustement de régressions logistiques pour explorer les associations entre antécédents pulmonaires et complications cardiaques.
- Étude complémentaire de l'association entre fibrillation ventriculaire et tachycardie ventriculaire par un test du khi-deux.

Les complications étudiées comprennent notamment les troubles du rythme, l'insuffisance cardiaque, la rupture du myocarde, le syndrome de dressler et la récidive d'infarctus.

## Fichiers à consulter

| Fichier | Contenu |
| --- | --- |
| [RAPPORT_ETUDE.pdf](RAPPORT_ETUDE.pdf) | Rapport final de l'étude |
| [DIAPORAMA_ETUDE.pdf](DIAPORAMA_ETUDE.pdf) | Support de présentation |
| [etude_myocard.Rmd](etude_myocard.Rmd) | Analyses et commentaires au format r markdown |
| [script_etude.R](script_etude.R) | Script d'analyse |
| [data/myocard.csv](data/myocard.csv) | Données utilisées |
| [Myocardial_infarction_complications_Database_description.pdf](Myocardial_infarction_complications_Database_description.pdf) | Description de la base et des variables |
| [ETUDE.Rproj](ETUDE.Rproj) | Projet rstudio |

## Utilisation

Les rapports pdf peuvent être consultés directement.

Pour travailler sur le code, installer r et rstudio, puis ouvrir `ETUDE.Rproj`. Depuis la racine du dépôt, installer les dépendances :

```r
install.packages(c("dplyr", "ggplot2", "rmarkdown", "knitr"))
```

Ouvrir `script_etude.R` pour exécuter les analyses par section. Pour générer une version html du document :

```r
rmarkdown::render("etude_myocard.Rmd", output_format = "html_document")
```

Le rendu nécessite pandoc, généralement fourni avec rstudio. Conserver le dossier `data/` à son emplacement : les chemins de lecture sont relatifs à la racine du projet.

## Contexte du travail

Travail réalisé par hâjar abdaoui, aisha helli et karima zouzane.

Les modèles explorent des associations dans les données étudiées. Leur interprétation ne permet pas, à elle seule, d'établir un lien de causalité. Le dépôt présente un travail académique d'analyse statistique.
