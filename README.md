# Maximum Entropy with Hidden Structure in R

## Harmonic Grammar in R
This repository provides R support for various batch and online computations in Maximum Entropy Harmonic Grammar. The repository can be used to find MaxEnt solutions for learning problems (with or without hidden structure), generate distributions over forms in different theories, and perform associated learning tasks.

### Installation
1. Install R: [https://cran.rstudio.com/](https://cran.rstudio.com/)
2. Clone the repository:
   ```sh
   git clone https://github.com/rstaubs/maxent-hidden-structure.git
3. Open the repository in your preferred R environment (e.g., RStudio).

### Examples
Example files are provided in the `examples` directory:

- **base**: Example driver file and data files for parallel MaxEnt (with and without hidden structure).
- **serial**: Example driver file and data files for serial MaxEnt.

### Notes on Optimization
The gradients used in this optimization are derived from the notes available [here](https://websites.umass.edu/hgr/files/2017/07/klnotes.pdf).

## Shiny App Implementation
The Shiny app provides a user-friendly interface for implementing the HGR model. The app offers additional functionalities, such as global normalization, which is not available in the standard HGR implementation. Through the Shiny app, users can quickly upload data, generate grammars, adjust constraint weights, and view results in real-time, making the process more efficient and accessible.

### Accessing the App
You can access the Shiny app online at [MaxEnt with Hidden Structure in R](https://alingwist.shinyapps.io).

### Running the App Locally
1. Install the necessary R packages:
   ```r
   install.packages(c("shiny", "DT", "shinyjs", "dplyr", "htmltools"))
2. Run the app using the following command:
   ```r
   shiny::runApp("shiny_app")

### Sample Input Files:
The Shiny app includes three sample input files, which can be found in the `sample_input_files` subdirectory. These files demonstrate different scenarios for using the MaxEnt model, including cases with or without hidden structures and cases of having output candidates with raw frequencies instead of normalized probabilities. these sample files can be uploaded directly into the Shiny app to see how the app handles different types of data.

### User Guide
#### Input Data Format
- The input file should be a CSV or TXT file with the following columns:
  - `input`: Input forms
  - `output`: Output forms
  - `probability`: Observed probabilities or raw frequencies
  - Additional columns representing various constraints.

#### Using the Shiny App
1. **Upload your data file**: Use the "Choose Input File" button to upload your CSV or TXT file.
2. **Select input type**: Choose between "Probabilities" and "Raw Frequencies".
3. **Select normalization type**: Choose between "Within Tableau" and "Global Normalization".
4. **Select prior type**: Choose between "L2" and "L1".
5. **Generate Grammar**: Click "Generate Grammar" to produce the grammar.
6. **Edit constraint weights**: If needed, adjust weights in the dynamic UI and click "Update Grammar" to recalculate probabilities and errors.
7. **Download outputs**: Use the "Download Output" button to save the generated tableau.

## Citations
- Staubs, Robert. 2011. Harmonic Grammar in R (hgR). Software package. Amherst, MA: University of Massachusetts Amherst. https://github.com/rstaubs/maxent-hidden-structure
- Nirheche, Ali. 2024. Shiny App for MaxEnt with Hidden Structure. Shiny Application. Amherst, MA: University of Massachusetts Amherst. https://alingwist.shinyapps.io