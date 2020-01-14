# TRACKIT+
### is a app for tracking resources and combat in a Dungeons and Dragons game.
***

The app aims to help with the chores that come with the **oldschool** style of the game, without removing its fun. Me and my friends started the campaign **Tomb of annihilation**, and the idea came up to make something to help track with the amount of resources and encumberment. The app is therefore specifically tailored for the jungle exploration but could be used to help with any kind of adventure and is especially useful for hex-crawlers alike.


## Getting started

It is build using _R Shiny_. The database is incorporated in _Firebase_.  

To start using it you either have to link the app to your database or create your project at firebase. You also have to create a project in google console and allowing authentication on the set url. If you run the app locally use _localhost:5555_ (Change the port number accordingly).


There are two files you need to create:
* secret.R  
  * store the secrets from Firebase and google (for reference look at dummy file)  
* dm.R  
  * here you store the ID of the game master. He also needs to create a character with **DM** as name.  


You also have to change the redirect path triggering on logging out or if the token expires. The path is found in `global.R`:

```
Shiny.addCustomMessageHandler('mymessage', function(message){window.location = 'http://localhost:5555';});
```


The easiest way to run the app is to have **Rstudio** from where you can lunch and edit the R code.  
You could run the app by running `runApp.R` as a R script, given you have the lyberis installed (you can find them at the top in `global.R`).

If you do not have R it is possible is to build a **docker** image

```
docker build -t trackit .
```
and launching the container.

```
docker run -p 5555:5555 trackit_1.0
```
