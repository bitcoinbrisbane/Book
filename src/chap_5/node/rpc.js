const express = require("express");

const app = express();
const PORT = 5000;

app.use(express.json());

app.post("/add", (req, res) => {
  const { operand1, operand2 } = req.body;
  const result = operand1 + operand2;
  res.json({ result });
});

app.listen(PORT);
