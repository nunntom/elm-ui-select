import { Elm } from "./src/Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("elm"),
  flags: Math.random().toString(36).substring(5),
});
