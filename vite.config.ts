/// <reference types="vitest" />
import rescript from "@jihchi/vite-plugin-rescript";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [rescript()],
  test: {
    include: ["test/**/*_test*.mjs"],
  },
});
