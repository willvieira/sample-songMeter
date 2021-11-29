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
# Boucle par sonomètre pour la selection des fichiers avec les etapes:
# 1. lister toutes les fichiers pour le sonomètre i
# 2. Classifier les fichiers dans le periode de nidification + periode de la journée (Aurore, Crépuscule, et Nuit)
# 3. Creer deux graphique pdf à l'interieur du dossier de chacun des sonomètres
# 4. 
# 5. 
###############################################################################

for(sono in sonometres)
{
    # imprimer dans la console le nom du sonometre pour vérifier le progress de la boucle
    cat('\n\n', paste0(rep('#', 50), collapse = ''), '\n  Sonomètre:', sono, '\n', paste0(rep('#', 50), collapse = ''))


    # lister toutes les fichiers pour le sonometre
    # `sizeRange_*` defini les limites min et max de taille (en KB) pour les fichiers 3 minutes et 10 minutes
    sono_dt <- listAudio(
        Dir = file.path(inputFolder, sono),
        sizeRange_3 = c(30000, 40000),
        sizeRange_10 = c(100000, 110000)
    )


    # Classifier la liste des fichiers dans les periodes de nidification et dans la journée (A1 - E6)
    # `groups` define en combien la period de temps (startDate - endDate) sera partagé
    sonoClass_dt <- classify_period(
        dt = sono_dt,
        startDate = lubridate::as_date('2020-05-30'),
        endDate = lubridate::as_date('2020-07-23'),
        groups = 5
    )


    # Échantillonner les fichiers
    # `sampleSize` et `overSample` pour chaque period de la journée ET groupe
    sono_echantillonne <- sample_files(
        dt = sonoClass_dt,
        sampleSize = 1,
        overSample = 1
    )


    # Générer une figure avec tous les fichiers distribué par periode de nidification et dans la journée
    # `colorDay` définit si on va différencier (TRUE) ou non (FALSE) les périodes de la journée avec une couleur
    # `outputFile` c'est le dossier avec le nom du fichier où la fonction va sauvegarder le pdf
    # la figure sera sauvegardé dedans le dossier du sonomètre
    plot_files(
        dt = sonoClass_dt,
        colorDay = TRUE,
        outputFile = file.path(outputFolder, sono, 'fichiersAudio.pdf')
    )


    # Générer une deuxième figure avec tous les fichiers + les fichiers echantillonés colorés
    plot_files(
        dt = sonoClass_dt,
        sampled = sono_echantillonne,
        colorDay = FALSE,
        outputFile = file.path(outputFolder, sono, 'fichiersAudio_echantillon.pdf')
    )


    # Déplacer les fichiers
    move_files(
        sampled = sono_echantillonne,
        input = inputFolder,
        output = outputFolder
    )

}
