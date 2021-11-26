################################################################################
# Instalation
################################################################################

# Installer les packages R necessaires si ils ne sont pas déjà installé
if (!require('lubridate')) install.packages('lubridate')
if (!require('suncalc')) install.packages('suncalc')
if (!require('tuneR')) install.packages('tuneR')
if (!require('lutz')) install.packages('lutz')
if (!require('rstudioapi')) install.packages('rstudioapi')

# Telecharger les fonctions necessaires
source('R/identify_recorder.R')
source('R/read_log.R')
source('R/classify_period.R')
source('R/sample_record.R')
source('R/plot_files.R')
source('R/move_samples.R')





################################################################################
# Définir les dossiers d'entrée et de sortie des fichiers audio
################################################################################

# Définir le dossier d'entrée (disque dur de stockage)
inputFolder <- rstudioapi::selectDirectory()

# Définir le dossier de sortir (où sauvegarder les audio sélectionnés?)
outputFolder <- rstudioapi::selectDirectory()


# Teste pour vérifier si les dossiers sélectionnés existent
for(int in c('input', 'output')) {
    if(!dir.exists(get(paste0(int, 'Folder')))) {
        cat(paste0('\u2717 Le `', int, 'Folder` ', 'est incorrect!\n'))
        stop(
            paste0("Le dossier `", get(paste0(int, 'Folder')), "` n'existe pas ou il n'est pas accessible")
        )
    }else{
        cat(paste0('\u2713 Le `', int, 'Folder` ', 'est correct!\n'))
    }
}





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




###############################################################################
# Première boucle par sonomètre pour vérifier si la classification et les échantillons sont corrects:
# - lister toutes les fichiers pour 1 sonomètre
# - Classifier les fichiers dans le periode de nidification + periode de la journée (Aurore, Crépuscule, et Nuit)
# - Creer deux graphique pdf à l'interieur du dossier de chacun des sonomètres
###############################################################################

for(sono in sonometres)
{
    # imprimer dans la console le nom du sonometre pour vérifier le progress de la boucle
    cat('\n\n', paste0(rep('#', 50), collapse = ''), '\n  Sonomètre:', sono, '\n', paste0(rep('#', 50), collapse = ''))

    # lister toutes les fichiers pour le sonometre (filtrer les fichiers avec plus de 34500 Kb)
    sono_dt <- listAudio_sm4(Dir = sono, filterSize = 34500)

    # Classifier la liste des fichiers dans les periodes de nidification et dans la journée (A1 - E6)
    sonoClass_dt <- classify_period(sono_dt,
                                    startDate = lubridate::as_date('2020-05-30'),
                                    endDate = lubridate::as_date('2020-07-23'),
                                    groups = 5)

    # Échantillonner les fichiers
    sono_echantillonne <- sample_files(sonoClass_dt, sampleSize = 1, overSample = 1)

    # Générer une figure avec tous les fichiers distribué par periode de nidification et dans la journée
    # `colorDay` définit si on va différencier (TRUE) ou non (FALSE) les périodes de la journée avec une couleur
    plot_files(sonoClass_dt, colorDay = TRUE)

    # Générer une deuxième figure avec tous les fichiers + les fichiers echantillonés colorés
    # `fileSuffix` consiste à ajouter un suffixe dans le nom du fichier pour éviter l'écrasement
    plot_files(sonoClass_dt, sono_echantillonne, colorDay = FALSE, fileSuffix = '_echantillon')
}




###############################################################################
# Dexième boucle par sonomètre pour la selection des fichiers:
# - lister toutes les fichiers pour 1 sonomètre
# - Classifier les fichiers dans le periode de nidification + periode de la journée (Aurore, Crépuscule, et Nuit)
# - Creer deux graphique pdf à l'interieur du dossier de chacun des sonomètres
###############################################################################

for(sono in sonometres)
{
    # imprimer dans la console le nom du sonometre pour vérifier le progress de la boucle
    cat('\n\n', paste0(rep('#', 50), collapse = ''), '\n  Sonomètre:', sono, '\n', paste0(rep('#', 50), collapse = ''))

    # lister toutes les fichiers pour le sonometre (filtrer les fichiers avec plus de 34500 Kb)
    sono_dt <- listAudio_sm4(Dir = sono, filterSize = 34500)

    # Classifier la liste des fichiers dans les periodes de nidification et dans la journée (A1 - E6)
    sonoClass_dt <- classify_period(sono_dt,
                                    startDate = lubridate::as_date('2020-05-30'),
                                    endDate = lubridate::as_date('2020-07-23'),
                                    groups = 5)

    # Échantillonner les fichiers
    sono_echantillonne <- sample_files(sonoClass_dt, sampleSize = 1, overSample = 1)

    # Générer une figure avec tous les fichiers distribué par periode de nidification et dans la journée
    # `colorDay` définit si on va différencier (TRUE) ou non (FALSE) les périodes de la journée avec une couleur
    plot_files(sonoClass_dt, colorDay = TRUE)

    # Générer une deuxième figure avec tous les fichiers + les fichiers echantillonés colorés
    # `fileSuffix` consiste à ajouter un suffixe dans le nom du fichier pour éviter l'écrasement
    plot_files(sonoClass_dt, sono_echantillonne, colorDay = FALSE, fileSuffix = '_echantillon')

    # Déplacer les fichiers
    move_files(sono_echantillonne)
}
