#!/usr/bin/env node

import { LspRocks } from "./lsp-rocks";

const [, , port] = process.argv;

new LspRocks(parseInt(port)).start();