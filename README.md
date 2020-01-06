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
