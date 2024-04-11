# doors

This repository holds analysis code in R for human behavioural data from a rules-based search task. 

### Cloning

Start by making a GitHub account! 'Fork' the doors repository, and create a personal access token with repository access. 

Next, create a local directory called 'doors'. Then, in Terminal, 

```bash
cd <directory>
git init
git remote add origin https://<username>:<PAT>@github.com/<username>/doors.git
```

The things in angle brackets are specific to you. You might end up with something like...

```bash
cd /Users/me/Documents/projects/doors
git init
git remote add origin https://ilovescience:d2840ajdilt49035iadf_PAT@github.com/ilovescience/doors.git
```

### The project

The project has these sub-directories:

- fig = figures showing individual and group results
- res = results, i.e. summary metrics extracted from raw data files stored elsewhere
- src = source code used to extract metrics and produce figures

If there are no folders called 'fig' and 'res', that's because Git is ignoring them. You will need to create your own locally.

### Running the code

- Open 'doors.Rproj'. This will open R with project-specific settings. 
- The first time you do this, you should be prompted to run `renv::restore()` to set up all the packages that the project needs. Enter 'y' to accept.

- Open 'run_wrangling.R'. This is the controlling script for 'get_data.R'. Together, they filter out excess information in our data files and sort them ready for analysis. 
- Update the data path

<!--I've written an absolute path for the data, which makes it easy for me to run the script on data that are stored outside the project directory. The path is specific to my computer, so you'll need to update it before you run the script. You can use your own absolute path (e.g. '\\C:\me\data\doors', '/Users/me/data/doors'), OR, if the data are inside the project, use file.path(project_path,'data'). Just make sure to add 'data' to your list of directories in .gitignore so git won't try to track or upload it!-->

- Select your settings

<!--You can choose which experiment version (task switching or transfer) and which session (learning, training, or test) you want to view, as well as whether you care about clicks or mouse position ('hover'). The 'version' variable changes the output file names to separate results from piloting and subsequent experiments. You could use e.g. 'piloting', 'study01', 'study02' etc. to distinguish sequential experiments, or name each experiment by the date on which it starts. The important thing is having a new identifier every time the task code changes (e.g. if you do a second round of experiments), so that we are always analysing data that belong together.-->

- Click anywhere in the script, and press CTRL+Shift+S or CMD+Shift+S to run!

- To create a plot of the results, open 'make_figs.R', update its settings as you did for run_analysis.R, and run.
