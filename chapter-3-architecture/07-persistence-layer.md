## Persistence layer

Our blockchain will need to save data of course. We have a number of different ways to persist data. All databases use files to store data, like SQLite, RocksDB or Postgres and MySQL.

MongoDB is a very popular, open source, NoSQL database. Instead of tables, Mongo uses documents that are binary JSON serialised, called BSON. This makes it very good for a web based application.

It uses data structures like hash maps to make retrieval of data very fast. For our poker application, we will persist users' balances as a key value pair: the user's public key and the balance of their account.

However, this does not easily allow us to perform analytics on our data, such as selecting the entire balance of all users. We could use an indexing service for that, should that be a requirement.

## Vector DBs

## Permissioned vs permissionless

Blockchains are sometimes categorized as permissioned or permissionless. We can think of a permission as the authority to modify or read its state.

## Comparison

For a bit of fun, and to begin a bit of coding, let's use our Docker knowledge and spin up 3 databases and then add some data and query them and compare the performance of each. We should consider the performance with different magnitudes of data.

- Redis
- Postgres
- MongoDB

Our `docker-compose.yaml` looks like this:

```yaml
services:
  postgres:
    image: postgres:15.3-bookworm
    container_name: postgres
    command:
      [
        "postgres",
        "-c",
        "log_statement=all",
        "-c",
        "log_destination=stderr",
        "-c",
        "log_min_messages=notice",
        "-c",
        "client_min_messages=notice"
      ]
    environment:
      POSTGRES_DB: book
      POSTGRES_USER: test
      POSTGRES_PASSWORD: Test1234
    ports:
      - "5432:5432"
  redis:
    image: redis:7.2.4-bookworm
    container_name: redis
    command: redis-server --requirepass Test1234
    ports:
      - "6379:6379"
  mongo:
    image: mongo
    restart: always
    environment:
      MONGO_INITDB_DATABASE: book
      # No auth for now
      # MONGO_INITDB_ROOT_USERNAME: root
      # MONGO_INITDB_ROOT_PASSWORD: example
    ports:
      - 27017:27017
```

Let's create three Node.js files — `redis.js`, `mongo.js` and `postgres.js` — after we init our project.

```bash
mkdir db_test && cd db_test

yarn init -y

touch docker-compose.yaml

touch redis.js && touch mongo.js && touch postgres.js
```

On each type of database, we will create an `Account` object and add 1,000 rows or documents to the db and use the Node.js `console.time` function to record the time taken to perform each operation. Running each script and recording the times from the JavaScript function … we see some rudimentary stats below.

Using the service flag on Docker we can start each of them from the single Docker file.

> **NOTE:** Waku is using Postgres now.

## Tearing down

Clean up volumes:

## Some other helpful Docker commands
