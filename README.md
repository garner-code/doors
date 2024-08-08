## The Study

In this study, we examined the effect of contextual clarity on task switching and learning transfer with a visual search task. This study contains two experiments: one measuring performance on task switching performance (exp_ts) and the other measuring learning transfer (exp_lt). The *transfer* variable in the dataset is not applicable to exp_ts.

The repository holds analysis code in R for human behavioural data. 

### Authors

Dinuk Welagedera

Emily Chung

Lydia Barnes

Kelly Garner

Christopher Nolan

### Task

Participants searched for target animals hiding in houses which contained 16 rooms. A house was represented by a 4 x 4 grid of squares, and animals were limited to appear behind one of 4 squares in each house.

### Data

**sub: subject number**

This variable contains the unique subject ID for each participant.

**ses: session**

Indicates which phase of the experiment that participants were in.

1 = Learning phase. The last 80 trials in the learning phase represent consolidation trials.

2 = Training phase

3 = Test phase

**t: trial number**

This variable records the specific trial number for each participant.

**context**

This variable identifies the house that participants have completed trials in. Each house is associated with a unique task set consisting of target doors which participants are required to learn. House 3 and 4 are only relevant to the learning transfer experiment.

1 = first learning house

2 = second learning house

3 = first transfer house

4 = second transfer house

**switch**

Records whether a particular trial is a switch trial (where the house/context has changed from the last trial), or a nonswitch trial (where the house/context remains the same as the last trial).

1 = switch trial

0 = nonswitch trial

**train_type**

Represents the type of training which participants received during the training phase.

1 = low switch group, where the probability of switch trials is 5% and the probability of nonswitch trials is 95%

2 = high switch group, where the probability of switch trials is 30% and the probability of nonswitch trials is 70%

NA = training type is not applicable (since participants in the learning phase still haven’t been allocated to either the high/low switch group)

**transfer**

Indicates the transfer condition that participants were completing.

1 = complete transfer condition. Target doors in the complete transfer condition were chosen from one of the two learning houses, and mapped identically onto a new grid with a different coloured border.

2 = partial transfer condition, where a combination of the target doors from the two initial learning houses are used. 2 target doors were randomly selected from each of the initial learning houses for this condition.

NA = transfer variable is not applicable (since participants had not reached the transfer stage of the experiment yet)

**n_clicks: number of clicks**

Number of door selections on a given trial.

**n_cc: number of context correct clicks**

Number of context-relevant door selections: those which are relevant to finding the target animal in the current house.

**n_oc: number of other-context correct clicks**

Number of context-irrelevant door selections: those which are relevant to finding the animal in the other house, which was not being shown on the current trial.

**accuracy**

Accuracy is the number of context-relevant door selections relative to the total number of door selections on a single trial. Values for this variable were calculated using the formula n_cc/n_clicks.

We used accuracy to determine how well participants had learned the target door locations for each context.

**rt: response time**

Defined as the time interval from the beginning of a trial (when a participant is able to first interact with the doors), to the first context relevant door selection. Values are measured in milliseconds.

We used response time as a measure of task switching performance. It provides an indication of how long it takes for participants to reload their representation of the doors on each trial. 

**win**

Win identifies the trials where the number of door selections were 4 or less.

TRUE = when number of door selections on a given trial was less than, or equal to 4.

FALSE - when number of selections on a given trial was greater than 4.

We used this variable to calculate the number of points earned in the training stage of the experiment. Participants were awarded points when the target animal was found within 4 moves.

**setting_errors**

Setting errors is the proportion of door selections which are not relevant to the current context, but are relevant to the other learning context, on a given trial. Setting errors are calculated using n_oc/n_clicks.

We used this variable to determine when participants were confusing the target doors for each house i.e., when participants were using target doors for the wrong context.

## The Code

### Cloning (optional)

If you want to follow along with the project's updates using git, you can make a GitHub account, 'fork' the doors repository to your account, and create a personal access token with repository access. 

Next, create a local directory called 'doors'. In Terminal, 

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

### Manual downloading (instead of cloning)

You can manually download the code instead, and skip cloning. To do that, go to the project on GitHub, click on the green 'Code' button, and select 'Download ZIP'. Just keep in mind that you will have to go back to GitHub and re-download the code to get the latest updates.

### Directories

The project has these sub-directories:

- fig = figures showing individual and group results
- res = results, i.e. summary metrics extracted from raw data files stored elsewhere
- src = source code used to extract metrics and produce figures

If there are no folders called 'fig' and 'res', that's because git is ignoring them. You will need to create your own locally. If you have cloned the repository and are pushing you're changes to GitHub, make sure you add the 'fig' and 'res' folders to your own .gitignore file to keep those aspects of the project private.

### Running the code

- Open 'doors.Rproj'. This will open R with project-specific settings. 
- The first time you do this, you should be prompted to run `renv::restore()` to set up all the packages that the project needs. Enter 'y' to accept.

- Open 'run_wrangling.R'. This is the controlling script for 'get_data.R'. Together, they filter out excess information in our data files and sort them ready for analysis. 
- Update the data path

> Look for a variable called 'data_path'. I've written an absolute path for the data, which makes it easy for me to run the script on data that are stored outside the project directory. The path is specific to my computer, so you'll need to update it before you run the script. Where it says '/Users/lydiabarnes/OneDrive - UNSW/task switch and transfer/data-sandpit', you will need to substitute your own path. 
>

- Select your settings

> You can choose which experiment version (task switching or transfer) and which session (learning, training, or test) you want to view, as well as whether you care about clicks or mouse position ('hover'). The 'version' variable changes the output file names to separate results from piloting and subsequent experiments. You could use e.g. 'piloting', 'study01', 'study02' etc. to distinguish sequential experiments, or name each experiment by the date on which it starts. The important thing is having a new identifier every time the task code changes (e.g. if you do a second round of experiments), so that we are always analysing data that belong together.
>

- Select all CMD+A (or CTRL+A) and press CMD+Enter (CTRL+Enter) to run!
- To create a plot of the results, open 'make_figs.R', update its settings as you did for run_analysis.R, and run.

### Editing the code

If you're feeling brave, you can...

Look inside get_data.R. This is where the raw files, with one row per sample (~every 20 ms), are loaded and filtered. 

Adjust the 'summary' steps in run_wrangling.R. These commands (e.g. `res <- grp_data %>% group_by(sub...) %>% summarise(switch = max(switch)...)` ) dictate what summaries you want for what aspects of your data. For example, we usually want to have separate output rows for each subject, so `group_by()` will almost always contain `sub`. You can remove `context` from `group_by` if you want to see someone's overall accuracy and don't care about whether it varied with context. The `summarise` function gets our averages and sums. If you want to know whether people were more likely to click in the current or other context, you could add a row to `summarise` along the lines of `set_accuracy = n_cc/n_oc` (number of clicks in current context compared to in other context). 

You can also filter the data before you do your summaries. For example, if you want to discard switch trials all together, you can add `res -> res %>% filter(switch==0)` after getting the results by trial, but before getting the results by subject. Filtering keeps only the rows that you've specified. In this example, it would keep rows on which there was no switch, which are marked as 0 in the data frame. 

You can try any and all these things just to see what happens. You can save the results to a file (click on the respective `write_csv(res,fnl)` and press CMD+Enter) and view them in Excel, or just run the thing you want to try (select only that text and press CMD+Enter) and view the res variable by clicking on it in the Environment (top right of your RStudio page). You can always go back to the code on GitHub if you don't like your modifications!
