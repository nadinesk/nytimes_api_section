I decided to try out exploring the New York Times API with R. For this example, I'm looking at their archives API. After parsing the data and seeing what was available and in what format, a good entry point seemed to be the Section Name variable. This post and the code below explores how the number of articles within different sections of the paper in a year has changed over ten years, from 2006 to 2016. 

One of the two function I use to explore this particular questions parses the data and creates a frequency table for each month. The other function loops through twelve months, and 