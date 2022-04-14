1. WRK_DRV_script - script de structuration des deux datasets info_course et result pour l'étude (feature engineering and filtre)

2. pairing_algo script et résultat du développement de l'aglorithme d'appariement homme/femme

3. MLM_fitting script et visualisation de sélection, ajustement et analyse de résultats du modèle GLMM

DRV_info_course.csv - unité une course sur une année
	- id_course_annee - clé primaire
	- rate_DNF_M - pourcentage de DNF homme
	- rate_DNF_F - pourcentage de DNF femme
	- rate_F - pourcentage participation des femmes
	- participation - nombre de coureur
	- temps_best - meilleur temps sur la course
	- diff_best - (vitesse meilleur Homme - vitesse meilleur Femme) / vitesse meilleur Femme
	- diff_3best - meme calcul avec les moyennes des 3 meilleur chrono homme et femme
	- diff_5perc - pareil avec 5% meilleur (seulement sur les course avec au moins 20 femmes
	- diff_mean - pareil avec la moyenne de tous les participants
	- nom_course
	- annee - annnée de la course
	- dist_final - distance de la course
	- deniv_final - D+ 
	- deniv_neg_final - D-
	- km_effort - km + D+(mètre) / 100
	- catUTMB - catégorie de km_effort 
	- continent 
	- cio - code pays 
	

DRV_result.csv - unité résultat d'un coureur sur une course sur une année
	- id_course_annee - clé étrangère
	- id_coureur - clé primaire
	- temps - format HH:MM:SS
	- place - classement
	- cote 
	- nationalite 
	- sexe
	- naissance - année de naissance
	- nom_course
	- annee - annnée de la course
	- dist_final - distance de la course
	- deniv_final - D+ 
	- deniv_neg_final - D-
	- km_effort - km + D+(mètre) / 100
	- catUTMB - catégorie de km_effort 
	- continent 
	- cio - code pays 
	- age - age du coureur au moment de la course
	- temps_s - temps en seconde 
	- vitesse_moy - vitesse moyenne du coureur sur la course