# doors

This repository holds analysis code in R for human behavioural data from a rules-based search task. 

### Cloning

To clone, create a local directory called 'doors'. In Terminal, 

```bash
cd <your new directory>
git init
git remote add origin https://lydiabarnes01:<PAT>@github.com/lydiabarnes01/doors.git
```

### Directories

- fig = figures showing individual and group results
- res = results, i.e. summary metrics extracted from raw data files stored elsewhere
- src = source code used to extract metrics and produce figures

### Setting up R

Download RStudio with the latest version of R. 

In R, 

```R
install.packages("renv")
renv::init(project = '/Users/lydiabarnes/Documents/academe/projects/doors',bare = TRUE)
```

Replace the project path with the path to your local copy!





