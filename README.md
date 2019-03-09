# AC-7

It includes these parts:

- The Haskell implementation of the NEAT algorithm
- An explorer to explore the population

## Run the Experiment
```
stack build
docker-compose up # then connect to the database and execute tool/pg-schema.sql
stack exec neat -- simulate
```

## Run the Explorer Only
```
docker-compose up
```

## The Explorer
![The Explorer](https://raw.githubusercontent.com/qwfy/AC-7/master/ui.png)
