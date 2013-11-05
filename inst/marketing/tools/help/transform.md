Change, create, add, and remove variables in the data.

#### Change

When you select the Change radio button a drop-down menu is shown that will allow you do apply common transformations to one or more variables in your data. For example to take the (natural) log of a variable select the variable you want to change and choose Log from the drop-down menu. A new variable is created with the prefix 'log.'. Click the 'Save changes' button to add the variable(s) to the dataset. A description of the transformation function included in Radyant is provided below.

#### Create

This is the most flexible command to create new or transformed variables. However, it also requires some knowledge or R syntax. A new variable can be any function of other variables in the data. Some examples are given below. In each example the name to the left of the '=' sign is the name of the new variable. On the right of the '=' sign you can include other variable names and basic R functions.

1. Create a new variable z that is the difference between variables x and y in the data

	z = x - y

2. Create a new variable z that is a transformation of variable x but with mean equal to zero (note that this transformation is available in the Change drop-down):

	z = x - mean(x)

3. Create a new factor z that takes on the value TRUE when x > y and FALSE otherwise

	z = x > y

4. Create a new factor z that takes on the value TRUE when x is equal to y and FALSE otherwise

	z = x == y

5. Create a variable z that is equal to x lagged by 3 periods

	z = shift(x,3)

#### Clipboard

It is possible to manipulate your data in Excel and copy-and-paste a new variable back into R. If you do not have the original data in Excel format use the 'clipboard' feature in Data > Manage to safe the data to the clipboard so you can paste it into Excel. Apply your transformations in Excel and then copy the new variable, with a header label, to the clipboard (i.e, CTRL-C on Windows and CMD-C on Mac). Click the Clipboard radio button and paste your new data into the 'Paste from Excel' box shown. It is key that the number of observations for the new variable is the same as in the original data. The new variable will be  shown on screen. To add the variable to the data click 'Save changes'.

#### Recode

To use the recode feature select the variable you want to change and click the 'Recode' radio button. Provide one or more recode commands (separate the command by a ';') and press enter to see the newly created variable. Click 'Save changes' to add the new variable to the data. Some recode command examples are given below.

1. All values below 20 are set to 'Low' and all others to 'High'

	lo:20 = 'High'; else = 'High'

2. Values 1 through 12 are set to 'A', 13:24 to 'B', and the remainder to 'C'

	1:12 = 'A'; 13:24 = 'B'; else = 'C'

3.	The transformation commands used for the Tulsa-Age cross-tab:

	'<25' = '<35'; '25-34' = '<35'; '35-44' = '35-54'; '45-54' = '35-54'; '55-64' = '>54'; '>64' = '>54'

4. To exclude a particular value (e.g., an outlier in the data) from subsequent analyses we can recode it to a missing value. If the largest value in a variable called __sales__ is, for example, equal to 400 we would (1) select the variable __sales__ in the 'Select column(s)' box and enter the command below in the 'Recode box': 

	400 = NA

#### Rename

Click the 'Rename' radio button and select one or more variables and enter new names for them. Separate each name by a ','. Press enter to see the variables with their new names on screen. Press 'Save changes' to alter the names in the original data.

#### Remove

Click the 'Remove' radio button and select one or more variables to remove from the selected data set. Press 'Save changes' to remove the variables from the original data. This action cannot be undone.

#### Transformations included in the Change function

1. Log: create a log-transformed version of the selected variable (i.e., log(x))
2. Square: multiply a variable by itself (i.e. x^2) 
3. Square-root: take the square-root of a variable (i.e., x^.5)
4. Center: create a new variable with a mean equal to zero (i.e., x - mean(x))
5. Standardize: create a new variable with a mean equal to zero and standard deviation equal to 1 (i.e., (x - mean(x)/sd(x)))
6. Invert: 1/x
7. Median split: create a new factor with two levels (Above and Below) that splits the variable values at the median
8. Deciles: create a new factor with 10 levels (deciles) that splits the variable values at the 10th, 20th, ..., 90th percentiles.
9. As factor: convert a variable to type factor (i.e., as.factor(x))
10. As number: convert a variable to type numeric (i.e., as.numeric(x))
11. As integer: convert a variable to type integer (i.e., as.integer(x))
12. As character: convert a variable to type character (i.e., as.character(x))
13. Rev factor order: the ordering of a factor is usually alphabetical, this function reverses that order which can be useful for regression analysis where the first level is used as the base level
14. As date (mdy): R will, by default, read dates as characters. Use this function if the dates are ordered as month-day-year
15. As date (dmy): R will, by default, read dates as characters. Use this function if the dates are ordered as day-month-year
16. As date (ymd): R will, by default, read dates as characters. Use this function if the dates are ordered as year-month-day
