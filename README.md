# disser_app

This is the repository with data and code to create Shiny application that allows looking at the results of my Master's thesis and making some fun simulations. 

To activate the application run first *library(shiny)* and after *runGitHub("disser_app","terabitiya")* in your R Studio. Very soon I will try to host it.

The key idea of this research is pretty simple. Previous scholars supposed that a voter deciding what political party to choose takes into account all the parties participating in a political campaign. However, it's not true. He or she has its own effective choice set. 

The model that is used in this research allows us to simulate the situation when a voter has some consideration set that we can create artificially. It is really appropriate to Russian political system with dominating party "United Russia" and restricted opposition.

So, to see how voters will behave conditioned on artificially created consideration sets try the application!

**Tip:**

Take the plots for "Democracy" political dimension. You can interpret the plots in the following way: the more value in ideology (X axis), the more democratic a voter. The more value of democracy, the more/less probability that the voter will choose the party. That's easy! 
