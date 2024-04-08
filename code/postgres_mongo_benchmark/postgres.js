const pg = require("pg");
const { Client } = pg;

const client = new Client({
  host: "localhost",
  user: "test",
  password: "Test1234",
  database: "book",
  port: 5432,
});

client.connect();
const ids = [];

const create_account_table = async () => {
  const query = {
    text: "CREATE TABLE IF NOT EXISTS account (id SERIAL PRIMARY KEY, address TEXT, balance INT)",
  };

  const result = await client.query(query).catch((error) => console.log(error));
  console.log(result);

  // Alter table and add index in address column
  const alter_query = {
    text: "CREATE INDEX IF NOT EXISTS address_index ON account(address)",
  };

  const alter_result = await client
    .query(alter_query)
    .catch((error) => console.log(error));

  console.log(alter_result);
};

const add_random_account = async () => {
  const address = Math.random().toString(36).substring(7);
  const balance = Math.floor(Math.random() * 100);

  const query = {
    text: "INSERT INTO account(address, balance) VALUES($1, $2)",
    values: [address, balance],
  };

  const result = await client.query(query);
  // console.log(result);
  ids.push(address);
};

const add_1000_accounts = async () => {
  await create_account_table();
  console.time("add_1000_accounts");
  for (let i = 0; i < 1000; i++) {
    await add_random_account();
  }
  console.timeEnd("add_1000_accounts");

  const random_index = Math.floor(Math.random() * 1000);
  const random_id = ids[random_index];

  console.time("select_1_account");
  const select_query = {
    text: "SELECT * FROM account WHERE address = $1",
    values: [random_id],
  };

  const select_result = await client.query(select_query);
  console.timeEnd("select_1_account");
};

console.log("Adding 1000 accounts");
add_1000_accounts();
console.log("Done");
