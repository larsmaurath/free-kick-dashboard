
# Lionel Messi Free Kick Explorer

The app visualizes all direct free kicks taken by Lionel Messi in the Spanish league for the seasons 06/07 to 18/19 and is based on free data from the provider Statsbomb. 

Four panels show the free kick locations, their goal mouth impact, a table with further information and either a freeze frame of the freekick situation or a summary of shot outcome. 

## Interactive Panels With Crosstalk

The four panels are linked via the [crosstalk](https://rstudio.github.io/crosstalk/) package that enables the sharing of user input between various shiny outputs. In this app the brushing of the two plots and the highlighting in the table filter an underlying data frame. If there is a change to this data frame all shiny outputs depended on it are updated.

## Use cases

### See how well Messi performs from free kicks in a certain region

Selecting a region in the top left plot shows where these free kicks ended up in the goal impact chart.
The table in the bottom left is filtered for these attempts and the bottom right shows a quick summary of their outcome, including an assessment on whether Messi has outperformed expectations in this region.

![](data/location.gif)

### Find all free kicks that ended up in the top left corner

Selecting a region in the top right plot restricts the analysis to all free kicks that were placed in this region of the goal.

![](data/goal_mouth.gif)

### Filter for all free kicks against a certain goalkeeper

The table on the bottom left can be filtered. Once rows are highlighted this flows thought to the other panels.

![](data/keeper.gif)

### Analyze the freeze frame for a single free kick

If a single free kick is selected the app shows a freeze frame instead of a summary in the bottom right. It shows the position of all players for the moment the ball was struck.

![](data/freeze_frame.gif)