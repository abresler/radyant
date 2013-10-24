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

http://www.r-bloggers.com/pander-0-3-8-is-out/
http://www.r-bloggers.com/how-to-put-your-meetup-on-the-map-literally/
http://www.r-bloggers.com/interactive-3d-in-shiny-shinyrgl/
https://groups.google.com/forum/?fromgroups=#!searchin/shiny-discuss/dynamic$20filter/shiny-discuss/je4zKWw0lBk/uAFCcVeulbEJ
https://groups.google.com/forum/#!topic/shiny-discuss/hzYGDJQF_e4
http://vita.had.co.nz/papers/tidy-data.pdf

Use data-tables once Shiny .8 hits CRAN https://github.com/rstudio/shiny-examples and http://glimmer.rstudio.com/yihui/12_datatables/


By David Gonzales:
"In my experience, unless the application is highly interactive and animated, the user prefers to wait until moving on to the next field or pressing <return> before the changes are updated. But still a user might like the reactivity of not having to mouse press a button."
https://groups.google.com/forum/#!topic/shiny-discuss/BFUgjICEQlc