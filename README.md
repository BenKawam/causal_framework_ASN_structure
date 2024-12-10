# A causal framework for the drivers of animal social network structure

## Organisation of the repository
We ran five studies. 
Four of them involved synthetic data. 
In the last one, we analysed an empirical data set collected in Assamese macaques.
The studies can be found in the Quarto notebooks that are located in the source directory. 
They are the central thread of this repository, and can be read/ran independently from one another.
The notebooks can be read as a _HTML_ files (see hyperlinks below) using any web browser, or can be opened in R as a _.qmd_ documents.
More information can be found in [*our manuscript*](https://www.biorxiv.org/content/10.1101/2024.06.26.600748v2).

-   [`01_random_features`](https://htmlpreview.github.io/?https://github.com/BenKawam/causal_framework_ASN_structure/blob/main/01_random_features.html) (simulation study 1). Random individual- and dyad-level network structuring features.
-   [`02_individual_features`](https://htmlpreview.github.io/?https://github.com/BenKawam/causal_framework_ASN_structure/blob/main/02_individual_features.html) (simulation studies 2 and 2'). Individual-level effects. 2: continuous variable (age). 2': discrete variable (sex).
-   [`03_dyad_features`](https://htmlpreview.github.io/?https://github.com/BenKawam/causal_framework_ASN_structure/blob/main/03_dyad_features.html) (simulation studies 3 and 3'). Dyad-level effects. 3: continuous variable (relatedness). 3': dicrete variable (sex).
-   [`04_conf_rel_rank`](https://htmlpreview.github.io/?https://github.com/BenKawam/causal_framework_ASN_structure/blob/main/04_conf_rel_rank.html) (simulation studies 4 and 4'). Confounded dyad-level effect. 4: genetic-relatedness, dominance rank. 4': genetic-relatedness, dominance rank, and group size.
-   [`05_models_for_empirical_data`](https://htmlpreview.github.io/?https://github.com/BenKawam/causal_framework_ASN_structure/blob/main/05_models_for_empirical_data.html): Fitting statistical model 4 (from notebook 4) to empirical data. Includes data visualisation, prior & posterior predictives, MCMC diagnostics, posterior contrasts.

### Folders
- `functions`: functions used in the Quarto notebooks (see details below).
- `stan_models`: stan models called in the the notebooks.
- `fitted_models`: posterior samples (tibble format) obtained in the notebooks. These files are *untracked* due to their large size.
- `data_assamese`: empirical data collected in a wild population of Assamese macaques.


## Common abbrevations used in the repository
-  SCM: Structural Causal Model.
-  SRM: Social Relations Model.


## Common functions
The functions listed below are used across notebooks.
-   `r.srm`: run *i* iterations of a stan model, and extract samples, for a set of parameters.
-   `fp`: plot *fixed* effect marginal *posterior* distributions, for one statistical model fitted to one dataset.
-   `fp.i`: plot *fixed* effect marginal *posterior* distributions, for one statistical model fitted to *i* iterations of a SCM.
-   `fp.i.split`: plot *fixed* effect marginal *posterior* distributions, for one statistical model fitted to *i* iterations of a SCM. The parameters are _splitted_ by types/bounds.
-   `fp.i.2`: plot *fixed* effect marginal *posterior* distributions, for *two* statistical models fitted to *i* iterations of a SCM. One (set of) panel(s) per statistical model.
-   `ivp.i`: plot *individual*-level *varying* effect marginal *posterior* distributions, for one statistical model fitted to one dataset.
-   `dvp.i`: plot directed *dyad*-level *varying* effect marginal *posterior* distributions, for one statistical model fitted to one dataset.
-   `post_m.i`: plot the marginal *posterior* distribution of directed rates *m*, for one statistical model fitted to one dataset.
-   `sn`: plot the *social network*, using either the observed interactions y or the true rate m, for one interation of the SCM.
-   `hist.plot`: plot histogram from a vector of real numbers.

