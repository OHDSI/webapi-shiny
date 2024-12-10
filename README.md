# Atlas Shiny Apps

This repo will contain the source code of Shiny apps that match the following analyses in Atlas

* Cohort Counts (apps/cohortCounts)
* Incidence Rates (apps/IncidenceRate)
* Cohort Characterization (apps/cohortCharacterization)
* Cohort Pathway (apps/cohortPathways)

These apps consume WebAPI output. The WebAPI output for a particular Atlas
analysis should be placed as text files (usually JSON) in the the /data/ folder of each app.
The app can then be deployed on a Shiny server.