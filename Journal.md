# lundi 27 janvier 2020, 11:20:15 (UTC+0100)

Corrigé simEasyPar pour conserver l'ordre entre les entrées et les sorties du scheduler.



# mercredi 22 janvier 2020, 11:59:54 (UTC+0100)

À vérifier: est-ce qu'il est important de conserver l'ordre des xs et ys avant et après le scheduler? Si oui, revoir les simulateurs. 

J'ai changé l'algorithme d'execution dans simPlasticPar pour qu'on puisse passer un état initial et que celui-ci soit mieux divisé pour créer les premiers runners. Tester et reporter à l'algo d'execution runPlasticPar, scanPlasticPar et éventuellement l'implém scala.

Fini d'implémenter et de tester le simulateur simPlasticPar.

Prochaine étape: 

- vérifier le problème de conservation de l'ordre des simulations dans simEasyPar.
- adapter la gestion de l'état initial dans run/scanPlasticPar et dans mgo.


# mardi 21 janvier 2020, 10:44:30 (UTC+0100)

Implémenter le simulateur de parallelisme puis tester la fonction `simEasyPar`.

Simulateur EasyPar implémenté et testé.

En cours: implémentation du simulateur pour PlasticPar


# lundi 20 janvier 2020, 15:49:18 (UTC+0100)

Implémenté une manière de mesurer le temps d'execution dans le module `Execution` avec les fonctions `evaluate` et `force`. À tester.

Il reste aussi à implémenter le simulateur de parallelisme `simScheduler` dans le même module. 


# mercredi 15 janvier 2020, 11:14:48 (UTC+0100)

Avant l'échantillonnage LHS, faire des tests plus petits: cas uniques pour illustrer les hypothèses sur l'efficacités de MonAPMC en choisissant les valeurs de paramètres (voir README). Pour réaliser ces expériences, il faut coder un simulateur d'execution d'algo en parallelisme pour mesurer le temps que prendrait un algo lancé en parallèle mais sans parallélisme. Ça me permettra de simuler l'execution d'un algo de 1 à 100 cœurs ou plus, pour pouvoir notamment mieux tester les cas ou le nombre de simulations de modèle par iteration est proche du nombre de cœurs (pour 1000 cœurs).


# jeudi 2 janvier 2020, 10:54:21 (UTC+0100)

Traduire MonAPMC en pseudo code dans README.md.

Première ébauche de traduction terminée. Il faudra plus tard uniformiser la notation et écrire les formules en latex.

Reprendre implémentation échantillonnage LHS des valeurs par (parallelisme), N / K (N correspond au nombre de simulation par iteration, ), V (la variance du temps de simulation).


# vendredi 20 décembre 2019, 11:24:53 (UTC+0100)

Implementer echantillonnage LHS des valeurs par (parallelisme), N / K (N correspond au nombre de simulation par iteration, ), V (la variance du temps de simulation).

En cours, traduction de MonAPMC en pseudo code dans README.md. Reprendre au calcul des poids dans la fonction step


# mardi 17 décembre 2019, 15:07:25 (UTC+0100)

Utilisation de fichiers sentinels dans le makefile pour éviter de tomber en enfer quand on execute make en parallèle.

Prochaine étape: réfléchir aux autres figures à générer, nettoyer le dossier report/.


# lundi 16 décembre 2019, 15:08:21 (UTC+0100)

Générer les spécifications de simulations hors du makefile.

Créé une règle `setup` dans le makefile qui s'occupe de générer la structure des répertoires nécessaires et les spécifications de simulations. Il faut executer `make setup` avant de pouvoir executer `make`.

Problème dans le fichier de statistiques pour L2 vs Nsimus. Vérifier les calculs.

Terminé la génération de la figure L2 vs NSimus. Reformattage des fichiers de données gnuplot pour utiliser la fonction de légende automatique (une ligne entre guillements au début de chaque bloc de données).

Prochaine étape: Réfléchir aux autres figures à générer. benchmark de la parallèlisation, etc.


# jeudi 12 décembre 2019, 10:51:38 (UTC+0100)

Écrire les règles du Makefile pour générer la figure L2 vs NSimus.

Écrit la règle pour la figure, les règles implicites pour les simulations et les règles pour la génération automatique des spécifications de simulation.

Problème: les règles implicites pour les simulations ne se déclenchent pas si les fichiers en dépendances (les spécifications de simulation) n'existent pas. Or, comme je voudrais que ceux-ci soit générés à la demande, automatiquement déclanchées quand je demande un résultat de simulation, ça ne marche pas.

Prochaine étaple: Générer les spécifications de simulations hors du makefile (avec un script et à l'aide des templates) pour être sûr qu'elles existent avant d'appeler make. 


# mercredi 11 décembre 2019, 14:16:03 (UTC+0100)

Fini commande MeanStdL2VsNSimu

Prochaine étape, avant d'implémenter le reste des commandes, écrire le Makefile pour générer toutes les figures déjà disponibles: steps et l2 vs nsimus

Problème dans l'écriture des fichiers histo: ils contiennent la même chose.

Résolu: dans la fonction HaskFile.writeListWith, j'utilise liftA2 sur des listes, ce qui combine chaque élément de la première liste (fichiers) à chaque élément de la seconde (histo). J'ai résolu le problème en utilisant des ZipList.

Prochaine étape: générer la figure L2 vs NSimus avec make.


# mardi 10 décembre 2019, 11:02:56 (UTC+0100)

Écrire commande "haskfile histosteps".

Fini commande histosteps.

En cours: écrire commande mean-std-l2-vs-nsimus, arrêté sur le calcul de moyenne et écarts types


# lundi 9 décembre 2019, 09:30:11 (UTC+0100)

Problème avec Cached: je n'ai pas d'instance Traversable ni monad, donc je ne peux pas faire ce que je veux avec des valeurs de type Cached (IO ()) que je récupère en sortie de simulation.

Réfactoring et simplification: Voir README.md. En pratique et résumé:
 
- utiliser haskell pour construire des executables qui font les simulations et les statistiques et écrire les fichiers de données au format gnuplot,
- utiliser gnuplot pour générer les figures,
- orchestrer tout ça avec make.

Prochaine étape: réaliser en haskell tous les executables spécifiés dans README.md, section "F: Haskell -> File".


