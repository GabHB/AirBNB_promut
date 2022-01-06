# AirBNB_promut

Version courte : 
J’ai choisi comme projet la création d’un modèle suggérant le prix d’affichage d’une nouvelle propriété. Ce projet n'est bien entendu qu’un squelette et plusieurs étapes importantes ont été omises par souci de temps. En prenant en considération l’ensemble de mes contraintes, il me semblait logique d’utiliser deux modèles assez simples, requérant peu de données et très facilement interprétables. J’ai donc choisi la régression et les forêts aléatoires avec méthode de validation croisée. J’ai d’abord effectué un tri rapide de plusieurs variables et j’ai gardé celles qui me semblaient pertinentes à première vue. J’ai par la suite travaillé les variables pour qu’elles soient pertinentes pour les modèles. J’ai ensuite construit les modèles et effectué quelques ajustements au niveau des variables pour ainsi finaliser mon projet. 

Version élaborée :
Lors de la remise du projet, sachant que mon temps était limité, j’ai eu l’idée de m’inspirer d’un projet que j’avais fait sur Kaggle. Le but était simplement de prédire le prix des maisons d’une région aux États-Unis à l’aide de plusieurs variables. Ayant cette idée en tête, j’ai tenté de voir s’il était possible et cohérent de créer un modèle similaire avec les données fournies. Rapidement, deux idées pertinentes sont surgies de mon esprit. Un modèle recommandant les meilleures propriétés pour le prix présentement disponible sur la plateforme et un modèle recommandant un prix pour les locateurs affichant une nouvelle habitation. Comme mon temps était limité, il me semblait logique d’opter pour la deuxième option qui semblait simplifier grandement le modèle. Entre autres, toutes les variables en lien avec les commentaires et les étoiles attribuées par les consommateurs n'étaient plus importantes, car la personne qui publie une nouvelle propriété ne possède pas encore de commentaires. De plus, cette décision semblait être cohérente d’un point de vue affaires. Commencer par un problème plus simple pour en prouver la plus-value et par le fait même se familiariser avec le jeu de données permet au scientifique de données de mieux estimer les problèmes qu’il pourra rencontrer et donc la durée d’une potentielle seconde phase. 
Un autre projet qui semblait intéressant est un modèle permettant de détecter les locateurs ayant des mauvaises notes et commentaires dans l’optique d’empêcher le plus rapidement ceux-ci d’afficher leurs propriétés sur AirBNB. L’idée est de bien entendu protéger les consommateurs, améliorer leur confiance envers le site web et améliorer la quantité de gens retournant sur la plateforme. 
Dans un autre ordre d’idée, le projet que j’ai choisi est malgré tout un projet qui pourrait prendre relativement beaucoup de temps à bâtir. Je n’ai donc pas eu la chance de compléter toutes les étapes que j’aurais accomplies en temps normal pour m’assurer d’un travail impeccable. J’ai entre autres retiré beaucoup de variables rapidement pour raccourcir la préparation des données qui est normalement l’étape la plus fastidieuse. De plus, je n’ai pas pris le temps de bien visualiser l’ensemble de mes données et d’effectuer des tests statistiques sur celles-ci. Finalement, j’aurais également aimé tester plus de modèles et effectuer plusieurs ajustements sur ceux déjà en place. Entre autres, simplement faire un XGBoost m’aurait paru pertinent sachant qu’il est tout autant interprétable que les deux modèles que j’ai utilisés. Vu la quantité limitée de données, j’aurais probablement considéré l’ajout de données des autres villes environnantes pour ainsi avoir un modèle plus solide et possiblement être en mesure d’utiliser plus de variables dans le but d’améliorer les performances du modèle. De plus, cet ajout permettrait possiblement l’utilisation d'un modèle plus complexe tel que les réseaux de neurones. Finalement, il y a aussi plusieurs facteurs que je n’ai pas pris en compte, tel que la saisonnalité. 



Conclusion, suite du projet : 
Après la finalisation d’un premier et d’un deuxième algorithme de forêt aléatoire, beaucoup d’idées se bousculent dans ma tête. Premièrement, avec peu d’effort, notre modèle semble nettement meilleur que la régression linéaire, sans trop de surprise. Autre constat, le modèle semble ne pas bien performer en général avec les données présentes. Selon moi, plusieurs éléments sont en cause, et quelques-uns ont été mentionnés plus haut. Entre autres, le champ amenities est une variable entrée manuellement qui est très inconsistante et qui mériterait beaucoup d’amour. La variable sur les quartiers est importante, mais devrait probablement être revue question de former moins de quartiers et accroître la pertinence de cette variable. De plus, l’état et l’apparence des lieux ne sont malheureusement pas des variables existantes qui influencent grandement le prix de location. C’est le type d’information que nous pourrions probablement retrouver dans les commentaires.

