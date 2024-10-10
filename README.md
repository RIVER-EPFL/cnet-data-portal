# CNET Data Portal

This Shiny app is meant to provide an easy interface for the CNET researchers to interact with their data. It can also be used as a data sharing portal accessible via the Web if wanted.

## Dependecies

Core dependencies and installation steps are defined in the `Dockerfile` assuming a Ubuntu installation.

- R version 4.0.2
- Node.js (tested version: 14.9.0)
- Terser.js (tested version: 5.3.0)

`renv` is used to manage package versions.

## Getting started

### Docker Compose

Using the supplied `docker-compose.yml` file, a cnet portal, MariaDB database and Traefik reverse proxy will start, simulating a complete deployment. The `Makefile` can be used to build and deploy, using the environment variables listed below in a populated `.env` file to customise the backend.

#### Environment variables

The environment variables are as follows:

```
CNET_ENV               // Whether the application is in `production` or `development`
CNET_DB_NAME           // The name of the database in MariaDB
CNET_DB_HOSTNAME       // The hostname of the MariaDB server
CNET_DB_PORT           // The port of the MariaDB server
CNET_DB_USERNAME       // The username the portal uses to access MariaDB
CNET_DB_PASSWORD       // The password the portal uses to access MariaDB
CNET_NOREPLY_ADDRESS   // A noreply email address to send from
CNET_TO_ADDRESS        // The email for contacting the site maintainer

---- Only needed if the MariaDB container is being used
CNET_DB_ROOT_PASSWORD  // The root password of the MySQL container
```

Note: If using a remote database, the docker compose file should be altered to disable
the MariaDB container and remove its dependence from the cnet portal container.


#### Volume mapping

The included `docker-compose.yml` maps the data, and db_backups
folders to the local filesystem. The folder `data` should contain the data
assets required to serve the the map datawithin the interface, and
the `db_backups` directory holds the database dumps.

```
- ./data:/srv/shiny-server/data
- ./db_backups:/srv/shiny-server/db_backups
```

#### Building and running

```bash
make run
```

By default, the reverse proxy will start on http port 80, and listen for connections to http://cnet.local. Add this hostname as a new line to your `/etc/hosts` system file, addressing your machine's local IP (`127.0.0.1`).

```bash
# Static table lookup for hostnames.

127.0.0.1 cnet.local
```

You should be able to access the site at [http://cnet.local](http://cnet.local).

### Ubuntu

The `Dockerfile` is built upon a Ubuntu image, therefore the installation steps for all dependencies can be follow from the instructions defined within.

### R and Rstudio
You need to install R and we recommend to use Rstudio as well. You can get the latest version of R or the recommend version forthis app on the CRAN website https://cran.r-project.org/ and Rstutio from their website https://rstudio.com/products/rstudio/download/.

### Node and Terser
To be able to parse and minify the cusom JavaScript files you will need to install Node.js and Terser.js as well.

To install Node please refer yourselfe to the documentation https://nodejs.org/en/download/.

Terser.js will need to be install globally to be accessible by the app. Once Node is installed run the following command:
```sh
npm install terser -g
```

## App oraganisation

### App.R
The main app script is the `App.R` file. It contains the basic app structure and logic to create and operate the main tabs and the initialization tasks.

### Modules
The app is organized and subdivised by tabs. Each tab and sub-tab is contained in a isolated module present in the `modules` directory. The structure of the `modules` directory should mirror the app structure. Some other reusable shiny components that aren't tabs or sub-tabs, but require both an UI and a server function, are also containerized in modules and located in a relevant place of the directory structure of the `modules` directory.

### Utils
Some reusable functions are organized by functionality in different files located in the `utils` directory.

| File                    | Description |
|:----------------------- |:----------- |
| calculation_functions.R | Contains all the functions necessary for the calculation of the parameters from the database that can be calculated based on other entered values. |
| data_preprocessing.R    | Contains all the functions used for the preprocessing of the sensor data loadded during the app startup. |
| db_interaction.R        | Contains the functions used to communicate with the _SQL_ database. |
| helper_functions.R      | Contains various functions that does not fit in the other files. |
| plotting_functions.R    | Contains the functions used to create ggplots. |
| shiny_extensions.R      | Contains the functions that extend _Shiny_ functionalities. |

### Assets
The custom stylesheet in _SCSS_ format and _JavaScript_ are located in the `sass` and `js` directories, respectively. These directories are located in the `assets` directory.

During development, both _SCSS_ and _JavaScript_ files are compiled and minified in two files, `main.css` and `metalpdataportal.js`, which are saved in the `www` directory.

To deploy the app in **production**, the assets **must be compiled manually** by running the `assets_compilation.R` script file from the app folder. This will create new `main.css` and `metalpdataportal.js` files tagged with the current timestamp to avoid browser cache conflicts.

When assets are compiled, a `assets_name.R` file is created/updated. This file is containing environment variables indicating the names of the two assets files and is sourced during app startup.

#### _SCSS_
In the `sass` directory, the `main.scss` file is an index that is used to load in the correct order all the partial files organized in different thematic directories.

#### _JavaScript_
The `js` directory contains all the _JavaScript_ code organized in different files by functionnality. The `manifest.json` file is used to compile the files together in a predefined order.

### Other _R_ script files

#### secrets.R
A `secrets.R` file **is required** and should contains all sensible information, such as DB name or password. These information are saved in environment variables when the file is sourced during the app startup. More info at https://github.com/mclement18/CNET-Portal-server/tree/master/app_deployment#create-or-update-secretr-file

#### packages_installation.R
The `packages_installation.R` file contains instructions to install the _R_ packages with the correct version. To install them, just run the script file. Currently, only the `dbplyr` package need a specific version for the app to work.

### Other directories

#### HTML components
The `html_components` directory contains all the HTML template used in the app.

#### www
The `www` directory is the public directory of the _Shiny_ app in which all the publicly accessible ressources must be put, such as favicon, images or assets.

#### Data
The `data` directory should contain the sensor data that are loaded during the app startup. For more specific information about its structure and the files needed, see comments in the `utils/data_preprocessing.R` and `modules/portal_management/portal_actions.R` files.

#### DB backups
The `db_backups` directory **should be present** and will contains all the _SQL_ database backups made with the DB backup functionnality of the portal actions module. See `modules/portal_management/portal_actions.R` file for more information.

## App deployment
Detail information on how to deploy this app on an _Ubuntu_ server can be found here: https://github.com/mclement18/CNET-Portal-server/tree/master/app_deployment

## To remove/add columns 
Delete from the respective table (data, etc), and ensure that the `grab_param_categories` does or does not contain the column name. The portal will get the columns from this table to populate the data grid. The function that is doing this is located in `/app/modules/data_management_tab/grab_data.R`:
```
    # Get the selected parameter category
    paramCat <- input$paramCategory
    # If all categories are selected
    if (paramCat == 'All') {
      # Get all categories
      columns <- getRows(
        pool,
        'grab_param_categories',
        columns = c('order', 'param_name')
      ) %>%
        pull(param_name)
    } else {
      # Else get only the selected one
      columns <- getRows(
        pool,
        'grab_param_categories',
        category == paramCat,
        columns = c('order', 'param_name')
      ) %>%
        pull(param_name)
    }
```
