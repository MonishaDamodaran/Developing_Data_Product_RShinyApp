# Developing_Data_Product_RShinyApp
This Shiny App is used to visualize basic univariate time series plots

The app name Univariate Time series Analysis signifies a very simple visualization of univariate time series data

The R dataset strong(AirPassengers) is used here. The dataset is first converted to a data frame object since it is a ts object. The year and index column is been mutated to AirPassengers data.


LEFT PANE
1) The left pane consists of a browse box to upload files,

2) The check box to get header of the imported files,

3) The radio button to select the type of imported files,

4) The drop down list displaying the data name strong(AirPassengers) by default and if the file has uploaded from your drive select upload files from the list,

5) The variable and group drop down boxes are column names of the selected data. It is used as a x axis and y axis for plots,

6) The Plot type box consists of option to get strong(Linegraph or None

7) Acf and Pacf plots are displayed in main panel by selecting the required one from acfpacf dropdown list,

8) The final checkbox gives the option to display the points in the plotted graph.


Once the app is opened, it displays the linegraph of the variable x vs x of AirPassengers data. Choosing the group to Index shows the actual line chart of Passengers data over time.

I have provided an example time series dataset called "Shampoo_data" in this github repo to try visualizing the plots using my app.
