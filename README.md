# GPS Art app set-up

## Clone this repo

```
git clone https://github.com/joebrew/stravart
```

## Set up credentials.yaml

Create a file called `credentials.yaml`. It should include the following fields
```
google_key: xxx
dbname: stravart
app_secret: xxx
rds_password: xxx
```

## Create database on AWS RDS
- In AWS RDS console, go to "Create database"
- Click "Easy Create"
- Click "PostgreSQL"
- Click "Free tier"
- Make "DB instance identifier" say "stravart-instance"
- Set "Master username" as "joebrew"
- Set the password to the `rds_password` field in the `credentials.yaml` file
- Set security group to default + `allwhite`
- In additional configuratin, make "initial database name" be `stravart`
- Keep other options same
- Confirm that you can connect by running the following:
```
psql --host stravart-instance.cfejspjhdciw.us-east-2.rds.amazonaws.com --port 5432 --username joebrew --dbname stravart
```
- When prompted for the password, use the `password` field in the `credentials.yaml` file

## Create a database locally for development
- Get into the postgres cli: `psql`
- Run: `create database stravart`

## Populate the local database
- Run the code in `db/set_up_database.R` to get the database populated locally

## Configure credentials to point at the right database
- For production, add the following fields to `credentials.yaml`:

```
dbname: stravart
host: xxx
port: 5432
user: xxx
password: xxx
```
-For local development, `dbname: stravart` is sufficient

## Populate the remote database

- Create a local dump:
```
pg_dump stravart > ~/Desktop/stravart.sql
```

- Transfer the dump to the remote db server:
```
psql -f stravart.sql --host stravart-instance.cfejspjhdciw.us-east-2.rds.amazonaws.com --port 5432 --username joebrew --dbname stravart
```
# To deploy to shiny server

- SSH into the server
- Install the relevant packages if not already installed

sudo apt-get install libxml2-dev
sudo apt-get install libjq-dev
sudo su - -c "R -e \"install.packages('leaflet.extras')\""
sudo su - -c "R -e \"install.packages('shinydashboard')\""
sudo su - -c "R -e \"install.packages('jqr')\""
sudo su - -c "R -e \"install.packages('sp')\""
sudo su - -c "R -e \"install.packages('rStrava')\""
sudo su - -c "R -e \"install.packages('tidyverse')\""
sudo su - -c "R -e \"install.packages('lubridate')\""
sudo su - -c "R -e \"install.packages('httr')\""
sudo su - -c "R -e \"install.packages('jsonlite')\""
sudo su - -c "R -e \"install.packages('glue')\""
sudo su - -c "R -e \"install.packages('DT')\""
sudo su - -c "R -e \"install.packages('leaflet')\""
sudo su - -c "R -e \"install.packages('highcharter')\""
sudo su - -c "R -e \"install.packages('shinyjs')\""
sudo su - -c "R -e \"install.packages('shinydashboard')\""
sudo su - -c "R -e \"install.packages('shinythemes')\""
sudo su - -c "R -e \"install.packages('logging')\""
sudo su - -c "R -e \"install.packages('googlePolylines')\""
sudo su - -c "R -e \"install.packages('tidyquant')\""
sudo su - -c "R -e \"install.packages('yaml')\""
sudo su - -c "R -e \"install.packages('dplyr')\""
sudo su - -c "R -e \"install.packages('RPostgreSQL')\""
sudo su - -c "R -e \"install.packages('readr')\""
sudo su - -c "R -e \"install.packages('DBI')\""
sudo su - -c "R -e \"install.packages('gridExtra')\""
sudo su - -c "R -e \"install.packages('revgeo')\""
sudo su - -c "R -e \"devtools::install_github('fawda123/rStrava', dependencies = TRUE, force = TRUE)\""



sudo systemctl restart shiny-server

- Make sure that the server is receiving on port 80 (default http)
- Edit the following file to do so: `/etc/shiny-server/shiny-server.conf`
- Change the `listen 3838;` to `listen 80;`
- Restart: `sudo service shiny-server restart`

### Deal with `.httr-oauth` token

Run the following locally in order to get a token

```
stoken <- httr::config(token = stravauth(app_name = 'GPSart',
                                                                   app_client_id = app_parameters$token_data$strava_app_client_id,
                                                                   app_secret = app_parameters$token_data$strava_app_secret,
                                                                   app_scope="activity:read_all",
                                                                redirect_uri = "http://localhost:8100/",
                                                                use_oob = FALSE,
                                                                cache = TRUE))
```

### On local machine

- Transfer the local app stuff to the remote machine:
```
scp -r -i "/home/joebrew/.ssh/openhdskey.pem" /home/joebrew/Documents/stravart ubuntu@bohemia.team:/home/ubuntu/Documents
```

### On remote machine

- Move things around and grant permissions:

```
sudo cp -r /home/ubuntu/Documents/stravart /srv/shiny-server/stravart;
cd /srv/shiny-server;
sudo chmod -R 777 stravart/;
sudo systemctl restart shiny-server;
```
