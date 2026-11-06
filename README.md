# NHL — but à chaque période

Site mis à jour automatiquement deux fois par jour (vers 8 h 15 et 17 h 45, heure de Paris) :

- **Ce soir** : matchs de la nuit, probabilité d'au moins un but dans chaque période (Poisson et empirique), forme des deux équipes sur leurs 10 derniers matchs, cote minimale, absences de joueurs clés.
- **Historique** : tous les matchs joués de la saison, avec le résultat du pari (gagné/perdu) et ce qu'aurait donné ta règle de sélection.
- **Équipes** : % de matchs avec un but par période, forme récente, joueurs clés.

Le filtre en haut de la page (forme mini, probabilité mini, modèle) s'applique aux trois onglets et est mémorisé dans ton navigateur.

## Contenu du dépôt

| Fichier | Rôle |
|---|---|
| `nhl_periods.R` | tout le calcul (le même script que sur ton PC) |
| `scripts/daily.R` | lancé chaque jour par GitHub : télécharge, prédit, exporte les données de la page |
| `.github/workflows/daily.yml` | la programmation automatique (horaires, installation de R, publication) |
| `site/index.html` | la page web |
| `site/data/` | données générées automatiquement (ne pas modifier) |
| `nhl_predictions_log.csv` | le log des prédictions, avec tes cotes |
| `plumber_api.R` | l'API locale, si tu veux encore l'utiliser sur ton PC |

## Mise en place (une seule fois, environ 10 minutes)

1. **Créer le dépôt** : sur github.com, *New repository*, nom par exemple `nhl-periodes`, visibilité **Public** (obligatoire pour GitHub Pages avec un compte gratuit), sans README.
2. **Déposer les fichiers** : dans le dépôt vide, *uploading an existing file*, puis glisse **tout le contenu** du dossier dézippé, y compris le dossier `.github`. Valide avec *Commit changes*.
   - Tu peux aussi y déposer ton `nhl_predictions_log.csv` actuel pour garder ton historique et tes cotes.
3. **Activer la page** : *Settings*, puis *Pages*, puis *Build and deployment*, puis *Source* : **GitHub Actions**.
4. **Autoriser l'écriture** : *Settings*, puis *Actions*, puis *General*, puis *Workflow permissions* : **Read and write permissions**, puis *Save*.
5. **Premier lancement** : onglet *Actions*, workflow **Daily NHL update**, puis *Run workflow*. Le premier passage télécharge toute la saison et prend environ 5 minutes.
6. **Ton site** : `https://TON-PSEUDO.github.io/nhl-periodes/` (l'adresse exacte s'affiche aussi dans *Settings*, puis *Pages*).

Ensuite, tout est automatique. Pour forcer une mise à jour (par exemple juste avant les matchs), relance *Run workflow*.

## Saisir tes cotes

Ouvre `nhl_predictions_log.csv` sur GitHub, clique sur le crayon (*Edit*), et remplis la colonne `odds_all3` de la ligne du match, par exemple `1,45`. Le fichier utilise des `;` comme séparateurs : garde-les.
Tu peux aussi le télécharger, le modifier dans Excel puis le redéposer (*Add file*, puis *Upload files*).
Le gain apparaît ensuite dans l'onglet Historique. Évite de modifier le fichier pendant qu'une mise à jour tourne (vers 8 h 15 et 17 h 45).

## Réglages

- **Ajouter la saison précédente** (plus de données) : dans `.github/workflows/daily.yml`, remplace `NHL_SEASONS: "20262027"` par `NHL_SEASONS: "20252026 20262027"`.
- **Changer les horaires** : lignes `cron` du même fichier (heures en UTC, donc l'heure de Paris moins 1 en hiver).
- **Seuils des joueurs clés** : `KEY_SHARE` (25 %) et `GOALIE_SHARE` (50 %) dans `nhl_periods.R`.

## Si quelque chose ne marche pas

- **Croix rouge dans l'onglet *Actions*** : clique dessus pour voir l'étape en échec et son message d'erreur.
- **La page affiche « Impossible de charger les données »** : le premier lancement n'a pas encore réussi (voir *Actions*).
- **Mises à jour arrêtées** : GitHub peut désactiver les tâches programmées d'un dépôt resté longtemps sans activité. Il envoie alors un e-mail, et il suffit de réactiver le workflow dans l'onglet *Actions*.
- **Dépôt public** : le contenu (log, cotes) est visible par toute personne qui a l'adresse.

Données : API publique de la NHL et rapport de blessures ESPN (non officiel, peut changer). Les tendances passées ne garantissent pas les résultats.
