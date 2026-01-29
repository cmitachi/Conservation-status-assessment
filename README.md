![Status](https://img.shields.io/badge/Status-Work_in_Progress-yellow)
![Completion](https://img.shields.io/badge/Completion-80%25-orange)

# Conservation status assessment for _Cerambyx cerdo_
This project analyzes the spatial distribution and mobility of the C. cerdo population using monitoring data and stochastic modeling.

## 🧰 Tools
1. QGIS (v. 3.32.0 "Lima")
   - mmqgis
   - QuickMapServices
2. RStudio (v. 2026.01.0+392 "Apple Blossom" Release (49fbea7a09a468fc4d1993ca376fd5b971cb58e3, 2026-01-04))
   - primer
   - RColorBrewer
   - readr 
   - deSolve
   - lattice
   - nls2
   - minpack.lm

## 📖 Methodology
The final report is being written through individual parts: species distribution maps, IUCN evaluation, Population Viability Analysis and a protected area proposal
### 🌎 QGIS
#### 🗺️ Maps and IUCN evaluation
The assessment followed the structure and methodology outlined in the IUCN Guidelines (2019), with the aim of producing a transparent and reproducible evaluation of the species’ conservation status. The first step was to compile all available distribution data (using **iNaturalist, GBIF and CKmap**) and apply the standardized **grid system**. This allowed the calculation of both the **Extent of Occurrence (EOO)** and the **Area of Occupancy (AOO)**, expressed in km², and the creation of a map visualizing these metrics. These spatial analyses provided the quantitative foundation for evaluating **Criterion B**. Once the distribution metrics were established, additional parameters required by the IUCN framework were assessed. After completing the species-specific assessment, the results were compared with existing national, regional, and global Red List evaluations. 

#### 📐 Protected area proposal
The database records were overlaid with the **Natura 2000 network** using the national Geoportal shapefiles, allowing to identify the biogeographical regions involved and quantify the proportion of occurrences falling within protected sites. This step was essential for evaluating whether current coverage meets the **EU guideline** of at least 50% representation per biogeographical region.

After assessing the existing protection, attention shifted to designing a **new reserve**. A geographically coherent cluster of populations was selected in **province of Matera** in Basilicata region to ensure that the proposed area addressed a real conservation gap. The reserve boundaries were drawn by considering habitat continuity, spatial configuration, and the six guiding principles of reserve design. Area, perimeter, and inter‑patch distances were calculated using attribute tables and spatial analysis tools. 

### 💻 RStudio
#### 🪲 Population Viability Analysis
The PVA is the EU approved method of evaluating extinction risk under different demographic and environmental scenarios. The first step was to organize all available demographic parameters such as survival, fecundity, and initial population size and translate them into a model structure consistent with standard PVA approaches to ensure biological rappresentability using a span of 100 years.

There were used both density-independent and density-dependent models. After the first evaluations, alternative scenarios were implemented. These alternative scenarios allowed to compare population trajectories, extinction probabilities, and sensitivity to specific parameters. The alternative scenarios used were
1. Forest fires
   - realistic percentage
   - pessimistic percentage
2. Deforestation
   - realistic percentage
   - pessimistic percentage
3. Synergic effect
   - realistic percentage
   - pessimistic percentage

The analysis provides a transparent, quantitative rationale for prioritizing interventions and understanding the species’ vulnerability.

## 📱 Contacts
>[![gmail](https://img.shields.io/badge/Gmail-D14836?style=for-the-badge&logo=Gmail&logoColor=white)](mailto:mitachicosmin@gmail.com)

>[![ProtonMail](https://img.shields.io/badge/proton%20mail-6D4AFF?style=for-the-badge&logo=protonmail&logoColor=white)](mailto:mitachi.cosmin@proton.me)

>[![linkedin](https://img.shields.io/badge/LinkedIn-0077B5?style=for-the-badge&logo=linkedin&logoColor=white)](https://www.linkedin.com/in/cosmin-george-mitachi-3b02101b7/)
