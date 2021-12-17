################################################################################
# Instalation
################################################################################

# Installer les packages R necessaires si ils ne sont pas déjà installé
if (!require('lubridate')) install.packages('lubridate')
if (!require('suncalc')) install.packages('suncalc')
if (!require('tuneR')) install.packages('tuneR')
if (!require('lutz')) install.packages('lutz')
if (!require('rstudioapi')) install.packages('rstudioapi')
if (!require('readxl')) install.packages('readxl')
if (!require('yaml')) install.packages('yaml')
if (!require('viridis')) install.packages('viridis')
if (!require('spsurvey')) install.packages('spsurvey')



# Telecharger les fonctions necessaires
source('R/identify_recorder.R')
source('R/read_log.R')
source('R/classify_period.R')
source('R/sample_record.R')
source('R/plot_files.R')
source('R/move_samples.R')
source('R/make_log.R')
source('R/inclusion_prob.R')






################################################################################
# Définir les dossiers d'entrée et de sortie des fichiers audio
################################################################################

# Définir le dossier d'entrée (disque dur de stockage)
inputFolder <- rstudioapi::selectDirectory()

# Définir le dossier de sortir (où sauvegarder les audio sélectionnés?)
outputFolder <- rstudioapi::selectDirectory()






################################################################################
# Extraire et valider les sonomètres dans le dossier d'entrée
################################################################################

# Extraire toute les dossiers à l'interieur du dossier inputFolder
sonometres <- dir(inputFolder)

# Voici toutes les dossiers trouvé:
print(sonometres)

# Si il y a des sonometres à enlever, les lister comme dans le example:
# sonometres_a_enlever <- c('710-607-1_ABC', '710-607-2_ABC', '...n sonomètre')
sonometres_a_enlever <- c('', '')

# Filter les sonometres à enlever
sonometres <- sonometres[!sonometres %in% sonometres_a_enlever]







################################################################################
# Definir les paramètres
################################################################################

durationRange_3 = c(177, 597)         # en secondes
durationRange_10 = c(597, 603)        # en secondes
date_debut = as_date('2020-05-30')
date_fin = as_date('2020-07-20')
sd = 7200                             # écart-type de la dist de probabilite autour du lever et coucher du soleil en seconds
cible = 20
cible_remplecement = 20
cible_validation = 5




###############################################################################
# Première boucle par sonometre pour
# 1. lister les fichiers
# 2. creer une figure pour visualiser les fichiers sauvegarde à l'inteirieur du dossier du sonometre de la memoir flash
# 3. Repeter les deux dernier etapes, mais avec un filtre sur la duré des audio
###############################################################################

for(sono in sonometres)
{
    # read song meter log
    sono_info <- yaml::read_yaml(
        file.path(
            inputFolder,
            sono,
            'log.yaml'
        )
    )

    # lister toutes les fichiers pour le sonometre
    # `sizeRange_*` defini les limites min et max de taille (en KB) pour les fichiers 3 minutes et 10 minutes
    # Si vous ne voulez pas faire de filtre, enlever les valeurs et ajouter un NA comme par example:
    # durationRange_3 = NA,
    # durationRange_10 = NA
    sono_dt <- listAudio(
        input = inputFolder,
        songMeter = sono,
        durationRange_3 = NA,
        durationRange_10 = NA,
        start_date = NA,
        end_date = NA,
        logMsg = FALSE
    )

    # Générer une figure avec tous les fichiers distribué par periode de nidification et dans la journée
    # `outputFile` c'est le dossier avec le nom du fichier où la fonction va sauvegarder le pdf
    # la figure sera sauvegardé dedans le dossier du sonomètre
    plot_files(
        dt = sono_dt,
        outputFile = file.path(inputFolder, sono, 'fichiersAudio.pdf')
    )

    # deuxième etape, mais avec un filtre pour les fichiers 3 et 10 min
    sono_dt <- listAudio(
        input = inputFolder,
        songMeter = sono,
        durationRange_3 = durationRange_3,
        durationRange_10 = durationRange_10,
        start_date = date_debut,
        end_date = date_fin,
        logMsg = FALSE
    )

    plot_files(
        dt = sono_dt,
        outputFile = file.path(inputFolder, sono, 'fichiersAudio_avecFiltre.pdf')
    )

    # calculer la probabilite d'inclusion
    # sd est la variance autour du lever et coucher du soleil en secondes
    sonoProb_dt <- incl_prob(
        dt = sono_dt,
        info = sono_info,
        sd = sd,
        logMsg = FALSE
    )

    # calculate probabilite d'inclusion
    sonoProb_dt$incl_prob <- (sonoProb_dt$incProb_sunrise + sonoProb_dt$incProb_sunset)/sum(sonoProb_dt$incProb_sunrise + sonoProb_dt$incProb_sunset)

    plot_files(
        dt = sonoProb_dt,
        outputFile = file.path(inputFolder, sono, 'fichiersAudio_avecFiltre_probInclusion.pdf')
    )
}








###############################################################################
# Boucle par sonomètre pour la selection des fichiers avec les etapes:
# 1. lister toutes les fichiers pour le sonomètre i
# 2. Classer les fichiers en période de nidification + période de jour (Aube, Crépuscule et Nuit)
# 3. Échantillons des fichiers audio par période de nidification et par période de jour
# 4. Créez deux graphiques pdf dans le dossier du sonomètre i pour illustrer tous les audio disponibles et ceux sélectionnés
# 5. Copier les fichiers sélectionnés dans le dossier de sortir
# 6. Générer un journal descriptif au format texte des fichiers audio sélectionnés, avec un résumé descriptif du plan d'échantillonnage
###############################################################################

for(sono in sonometres)
{
    # imprimer dans la console le nom du sonometre pour vérifier le progress de la boucle
    cat('\n\n', paste0(rep('#', 50), collapse = ''), '\n  Sonomètre:', sono, '\n', paste0(rep('#', 50), collapse = ''))


    # read song meter log
    sono_info <- yaml::read_yaml(
        file.path(
            inputFolder,
            sono,
            'log.yaml'
        )
    )

    # Créer un dossier 3 et 10 min pour chaque sonomètres
    # À l'interieur de chaque Dossier_sonometre, il aurait un dossier 3 min et un 10 min. Les fichiers selectionnés seront mis dedans chacun de ces dossiers, et les fichiers supplementaire dans un sub dossier
    invisible(
        sapply(
            file.path(
                outputFolder,
                sono,
                c(
                    file.path(
                        c('audio_3min', 'audio_10min'),
                        'selection_remplacement'
                    )
                )
            ),
            dir.create, recursive = TRUE
        )
    )


    # lister toutes les fichiers pour le sonometre
    # `sizeRange_*` defini les limites min et max de taille (en KB) pour les fichiers 3 minutes et 10 minutes
    sono_dt <- listAudio(
        input = inputFolder,
        songMeter = sono,
        durationRange_3 = durationRange_3,
        durationRange_10 = durationRange_10,
        start_date = date_debut,
        end_date = date_fin
    )



    # calculer la probabilite d'inclusion
    # sd est la variance autour du lever et coucher du soleil en secondes
    sonoProb_dt <- incl_prob(
        dt = sono_dt,
        info = sono_info,
        sd = sd
    )



    # Échantillonner les fichiers
    # `sampleSize` et `overSample` pour chaque period de la journée ET groupe
    sono_selection <- sample_GRTS(
        dt = sonoProb_dt,
        sampleSize = cible,
        overSample = cible_remplecement
    )


    # Générer une figure avec tous les fichiers distribué par periode de nidification et dans la journée
    # `outputFile` c'est le dossier avec le nom du fichier où la fonction va sauvegarder le pdf
    # la figure sera sauvegardé dedans le dossier du sonomètre
    plot_files(
        dt = sono_selection,
        outputFile = file.path(outputFolder, sono, 'fichiers_selection.pdf')
    )
 

    # Copier les fichiers sélectionnés
    move_files(
        sampled = sono_selection,
        input = inputFolder,
        output = outputFolder
    )


    # Générer la liste des fichiers selectionés
    make_list(
        sampled = sono_selection,
        nbValidation = cible_validation,
        output = file.path(outputFolder, sono)
    )

}
