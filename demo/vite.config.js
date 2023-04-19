import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  base: "",
  build: {
    target: "es2015",
  },
});