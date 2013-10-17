Dynamic filters for View, Visual, and Explore. Depending on what variable is selected a control pops up
- For numeric a range slider
- For factor a check box list
- For date a slider with a time-line

What if you select multiple filter variables? I guess you could put up a selectInput for numeric/date/factor separately. If a variable is selected then the appropriate control is created.

Discussion on dynamic input-elements:
https://groups.google.com/forum/?fromgroups=#!searchin/shiny-discuss/dynamic$20filter/shiny-discuss/je4zKWw0lBk/uAFCcVeulbEJ

Avoid eval(parse()).

Using plyr until dplyr can be used as either (1) a substitute for plyr or (2) together with plyr

To implement a filter on the data for all analyses put something in the getdata() function. By the way ... shouldn't that be a reactive function? Especially if you apply a filter right?

Dataview changes:
- Explore -- add subset commands etc. 
- Visualize -- more plotting options