# mardi 17 décembre 2019, 15:07:25 (UTC+0100)

Utilisation de fichiers sentinels dans le makefile pour éviter de tomber en enfer quand on execute make en parallèle.


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


