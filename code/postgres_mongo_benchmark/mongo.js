const mongoose = require("mongoose");
const mongoString = "mongodb://127.0.0.1:27017/book";

mongoose.connect(mongoString);
const database = mongoose.connection;

database.on("error", (error) => {
  console.log(error);
});

database.once("connected", () => {
  console.log("Database Connected");
});

const accountSchema = new mongoose.Schema({
  address: {
    required: true,
    type: String,
  },
  balance: {
    required: true,
    type: Number,
  },
});

const Account = mongoose.model("Account", accountSchema);

const add_random_account = async () => {
  // console.time("add_1_account");
  const account = new Account({
    address: Math.random().toString(36).substring(7),
    balance: Math.floor(Math.random() * 100),
  });

  const result = await account.save();
  // console.timeEnd("add_1_account");
  // console.log(result._id);
};

const add_1000_accounts = async () => {
  console.time("add_1000_accounts");
  for (let i = 0; i < 1000; i++) {
    await add_random_account();
  }
  console.timeEnd("add_1000_accounts");
};

console.log("Adding 1000 accounts");
add_1000_accounts();
console.log("Done");
