// add redis client

const redis = require("redis");
const client = redis.createClient({ url: "redis://localhost:6379", username: "default", password: "Test1234"});

client.on("error", (error) => {
  console.log(error);
});

client.on("connect", () => {
  console.log("Connected to Redis");
});

const add_random_account = async () => {
  const account = {
    address: Math.random().toString(36).substring(7),
    balance: Math.floor(Math.random() * 100),
  };

  client.hSet(account.address, account, (error, result) => {
    if (error) {
      console.log(error);
    }
  });
};

const add_1000_accounts = async () => {
  await client.connect();
  console.time("add_1000_accounts");
  for (let i = 0; i < 1000; i++) {
    await add_random_account();
  }
  console.timeEnd("add_1000_accounts");
};

console.log("Adding 1000 accounts");
add_1000_accounts();
console.log("Done");
