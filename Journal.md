lundi 9 décembre 2019, 09:30:11 (UTC+0100)
==========================================


Problème avec Cached: je n'ai pas d'instance Traversable ni monad, donc je ne peux pas faire ce que je veux avec des valeurs de type Cached (IO ()) que je récupère en sortie de simulation.

Réfactoring et simplification: Voir README.md. En pratique et résumé:
 
- utiliser haskell pour construire des executables qui font les simulations et les statistiques et écrire les fichiers de données au format gnuplot,
- utiliser gnuplot pour générer les figures,
- orchestrer tout ça avec make.

Prochaine étape: réaliser en haskell tous les executables spécifiés dans README.md, section "F: Haskell -> File".


mardi 10 décembre 2019, 11:02:56 (UTC+0100)
===========================================


Écrire commande "haskfile histosteps".

Fini commande histosteps.

En cours: écrire commande mean-std-l2-vs-nsimus, arrêté sur le calcul de moyenne et écarts types


mercredi 11 décembre 2019, 14:16:03 (UTC+0100)
==============================================


Fini commande MeanStdL2VsNSimu

Prochaine étape, avant d'implémenter le reste des commandes, écrire le Makefile pour générer toutes les figures déjà disponibles: steps et l2 vs nsimus

Problème dans l'écriture des fichiers histo: ils contiennent la même chose.

Résolu: dans la fonction HaskFile.writeListWith, j'utilise liftA2 sur des listes, ce qui combine chaque élément de la première liste (fichiers) à chaque élément de la seconde (histo). J'ai résolu le problème en utilisant des ZipList.

Prochaine étape: générer la figure L2 vs NSimus avec make.


jeudi 12 décembre 2019, 10:51:38 (UTC+0100)
===========================================


Écrire les règles du Makefile pour générer la figure L2 vs NSimus.

Écrit la règle pour la figure, les règles implicites pour les simulations et les règles pour la génération automatique des spécifications de simulation.

Problème: les règles implicites pour les simulations ne se déclenchent pas si les fichiers en dépendances (les spécifications de simulation) n'existent pas. Or, comme je voudrais que ceux-ci soit générés à la demande, automatiquement déclanchées quand je demande un résultat de simulation, ça ne marche pas.

Prochaine étaple: Générer les spécifications de simulations hors du makefile (avec un script et à l'aide des templates) pour être sûr qu'elles existent avant d'appeler make. 


lundi 16 décembre 2019, 15:08:21 (UTC+0100)
===========================================


Générer les spécifications de simulations hors du makefile.

Créé une règle `setup` dans le makefile qui s'occupe de générer la structure des répertoires nécessaires et les spécifications de simulations. Il faut executer `make setup` avant de pouvoir executer `make`.

Problème dans le fichier de statistiques pour L2 vs Nsimus. Vérifier les calculs.

Terminé la génération de la figure L2 vs NSimus. Reformattage des fichiers de données gnuplot pour utiliser la fonction de légende automatique (une ligne entre guillements au début de chaque bloc de données).

Prochaine étape: Réfléchir aux autres figures à générer. benchmark de la parallèlisation, etc.


mardi 17 décembre 2019, 15:07:25 (UTC+0100)
===========================================


Utilisation de fichiers sentinels dans le makefile pour éviter de tomber en enfer quand on execute make en parallèle.

Prochaine étape: réfléchir aux autres figures à générer, nettoyer le dossier report/.


vendredi 20 décembre 2019, 11:24:53 (UTC+0100)
==============================================


Implementer echantillonnage LHS des valeurs par (parallelisme), N / K (N correspond au nombre de simulation par iteration, ), V (la variance du temps de simulation).

En cours, traduction de MonAPMC en pseudo code dans README.md. Reprendre au calcul des poids dans la fonction step


jeudi 2 janvier 2020, 10:54:21 (UTC+0100)
=========================================


Traduire MonAPMC en pseudo code dans README.md.

Première ébauche de traduction terminée. Il faudra plus tard uniformiser la notation et écrire les formules en latex.

Reprendre implémentation échantillonnage LHS des valeurs par (parallelisme), N / K (N correspond au nombre de simulation par iteration, ), V (la variance du temps de simulation).


mercredi 15 janvier 2020, 11:14:48 (UTC+0100)
=============================================


Avant l'échantillonnage LHS, faire des tests plus petits: cas uniques pour illustrer les hypothèses sur l'efficacités de MonAPMC en choisissant les valeurs de paramètres (voir README). Pour réaliser ces expériences, il faut coder un simulateur d'execution d'algo en parallelisme pour mesurer le temps que prendrait un algo lancé en parallèle mais sans parallélisme. Ça me permettra de simuler l'execution d'un algo de 1 à 100 cœurs ou plus, pour pouvoir notamment mieux tester les cas ou le nombre de simulations de modèle par iteration est proche du nombre de cœurs (pour 1000 cœurs).


lundi 20 janvier 2020, 15:49:18 (UTC+0100)
==========================================


Implémenté une manière de mesurer le temps d'execution dans le module `Execution` avec les fonctions `evaluate` et `force`. À tester.

Il reste aussi à implémenter le simulateur de parallelisme `simScheduler` dans le même module. 


mardi 21 janvier 2020, 10:44:30 (UTC+0100)
==========================================

Implémenter le simulateur de parallelisme puis tester la fonction `simEasyPar`.

Simulateur EasyPar implémenté et testé.

En cours: implémentation du simulateur pour PlasticPar


mercredi 22 janvier 2020, 11:59:54 (UTC+0100)
=============================================

À vérifier: est-ce qu'il est important de conserver l'ordre des xs et ys avant et après le scheduler? Si oui, revoir les simulateurs. 

J'ai changé l'algorithme d'execution dans simPlasticPar pour qu'on puisse passer un état initial et que celui-ci soit mieux divisé pour créer les premiers runners. Tester et reporter à l'algo d'execution runPlasticPar, scanPlasticPar et éventuellement l'implém scala.

Fini d'implémenter et de tester le simulateur simPlasticPar.

Prochaine étape: 

- vérifier le problème de conservation de l'ordre des simulations dans simEasyPar.
- adapter la gestion de l'état initial dans run/scanPlasticPar et dans mgo.


lundi 27 janvier 2020, 11:20:15 (UTC+0100)
==========================================

Corrigé simEasyPar pour conserver l'ordre entre les entrées et les sorties du scheduler.

Supprimé l'utilisation de scanPlasticPar pour garder uniquement l'algo implémenté dans simPlasticPar. Le premier devrait être mis à jour avec le dernier si il faut utiliser la fonction.

Prochaine étape: faire les simulations les graphes nécessaire pour visualiser la valeur de L2 en fonction du parallélisme, du ratio du nombre de simulations par rapport au nombre de cœurs, et de la variance de la durée de simulation. On fait varier un paramètre à la fois selon des valeurs choisies, et les valeurs par défaut des paramètres sont: n=5000, nAlpha=500, pAccMin=0.01, parallel=1, stepSize=1, stopSampleSize=4500.


vendredi 31 janvier 2020, 18:33:05 (UTC+0100)
=============================================

Préparé le makefile pour faire les nouvelles simulations: simu + stats + figure.

Refactoré le code haskell pour utiliser simEasyPar et simPlasticPar partout. Les tests passent.

À faire: 

- coder en haskell l'executable qui sert à calculer les stats L2 vs time pour les replications (voir la règle du makefile pour `files_stat_l2_vs_time_k`)
- coder le script gnuplot


lundi 3 février 2020, 09:22:08 (UTC+0100)
=========================================

Revoir l'organisation du code haskell pour refléter l'organisation en "définitions/valeurs" - "fichiers" comme dans le makefile.

Codé les stats L2 vs Time pour des replications de steps, créé l'executable correspondant et intégré au Makefile dans la section Stat L2 vs time. 

Faire le script gnuplot pour Générer la figure L2 vs time K.

Scrit fait et figure Générée.

Trouver un cas où MonAPMC est vraiment plus efficace que APMC (voir cas idéal dans le readme.)


mardi 4 février 2020, 10:14:33 (UTC+0100)
=========================================

Automatisé la création des fichiers de specification de simulation dans input/simu avec le `Makefile` et le script `util/populate_simu_specs.sh`.

Les noms des paramètres d'APMC et MonAPMC n et nAlpha sont confus. Adaptation des noms pour faire directement référence à la taille de l'échantillon (nAlpha) et le nombre de simulation effectuées à chaque étape (nGen).

Changé le dernière valeur de K dans Fig L2 vs Time K pour 100 au lieu de 10. Il a fallu aussi adapter la valeur de stepMax pour que les deux algos fassent au max le même nombre de simulations.


jeudi 6 février 2020, 15:45:37 (UTC+0100)
=========================================

Pour sampler une distribution gamma (utilisé dans le modèle avec variance du temps d'exécution), je n'ai trouvé que la bibliothèque statistics qui utilise System.Random.MWC (il y a aussi Data.Random mais je ne sais pas à quoi correspondent les paramètres de la distribution gamma). J'utilise cette bibliothèque dans Distribution.gammaRandomSample. Pour l'instant, la méthode doit créer une nouvelle seed à chaque appel à partir d'un générateur StdGen, ce qui est lent et/ou peu robuste statistiquement puisqu'on perd les propriétés du générateur MWC. TODO: il faudra passer à Problème: un générateur MWC est mutable, on ne peut donc pas l'utiliser dans plusieurs threads en même temps, attention à la parallelisation. (En préparation de l'utilisation de MWC partout, je l'ai mis aussi dans Distribution.normalRandomSample.)


vendredi 7 février 2020, 17:32:47 (UTC+0100)
============================================

Le script `util/populate_simu_specs.sh` n'est pas facile à adapter pour les noms de fichiers plus complexes avec les nouveaux modèles (`..._modelToyTimeVar_1_1_...`). Pour les traiter plus facilement, j'ai écrit un executable en haskell qui parse les noms de fichiers pour construire la spec désirée.

Implémentation des modèles au temps d'execution variable ToyTimeBias et ToyTimeVar. ToyTimeBias est paramêtré avec la moyenne et la variance pour pouvoir faire varier cette dernière en gardant constant le temps moyen d'execution d'un modèle, et donc le temps total d'execution des modèles.

La figure L2 vs Time K V montre comme on s'attendait qu'APMC offre un avantage sur APMC quand à la fois le parallelisme et la variance de temps de calcul sont élevés.


lundi 10 février 2020, 10:45:49 (UTC+0100)
==========================================

J'ai complété la figure L2 vs Time K V en montrant aussi le cas pour chaque algo où le temps d'execution du modèle est dépendant des valeurs de paramètres de l'algo (ToyTimeBias), ce qui pourrait introduire un biais. La Figure Fig Steps Bias montre aussi l'histogramme de l'échantillon final pour APMC, MonApmc avec nGen = 40 et MonAPMC avec nGen = 1. Pas de biais visible.


jeudi 30 avril 2020, 17:59:37 (UTC+0200)
========================================

Refactoring du Makefile pour que les paramètres qui contrôlent chaque figure soient mieux localisés ensemble et ne pas avoir à changer plusieurs parties du fichier (fichiers de stats et fichiers de simus par exemples) quand on veut modifier une figure. Pour cela, j'ai créé une unique liste des simulations à laquelle les différentes parties du fichier se réfèrent en utilisant la fonction make `foreach`. Harmonisation du format des recettes en "input, output, sentinel"

Dans, L2 vs time K V, le L2 est élevé pour le cas ou le temps d'execution du modèle est biaisé. C'est que le L2 est calculé par rapport à une distribution théorique gaussienne! Il faut corriger ça.

lundi 4 mai 2020, 18:48:44 (UTC+0200)
=====================================

Corrigé la distribution de référence pour le calcul du L2 avec le modèle uniforme ToyTimeBias. On retrouve bien des valeurs de L2 attendues dans la figure L2 vs time K V.

Attention à bien recompiler les executables haskell quand je retouche du code, et à bien prendre les nouvelles versions.
