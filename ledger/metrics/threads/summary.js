#!/usr/bin/env node

// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

const readline = require('readline');
const fs = require('fs');

const args = process.argv.slice(2);
if (args.length === 1) {
    const logfilename = args[0];
    run(logfilename);
} else {
    console.log("Usage: ./summary.js sandbox.log")
}

function addSample(map, key1, key2) {
    const map2 = map.get(key1) || new Map();
    const value = map2.get(key2) || 0;

    map2.set(key2, value + 1);
    map.set(key1, map2);
}

async function run(path) {
    const fileStream = fs.createReadStream(path);

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const methodToThreads = new Map();
    const threadToMethods = new Map();

    for await (const line of rl) {
        const found = line.match(/^.*\[(.*)-[0-9]+\] TRACE (.*) - \[TRACE-THREADS\] (.*)$/)
        if (found) {
            const [_match, thread, _qualifiedClass, method] = found;

            addSample(methodToThreads, method, thread);
            addSample(threadToMethods, thread, method);
        }
    }

    const methodNames = [...methodToThreads.keys()].sort();
    const threadNames = [...threadToMethods.keys()].sort();
    const maxMethodLength = methodNames.reduce((maxLength, name) => Math.max(maxLength, name.length), 0);

    console.log("=== All methods ===");
    methodNames.forEach(method => {
        const threads = methodToThreads.get(method)
        const threadNames = [...threads.keys()].sort();
        console.log(`${method.padEnd(maxMethodLength, " ")} ${threadNames.map(thread => `${thread} [${threads.get(thread)}]`).join(", ")}`)
    })
    console.log("");

    threadNames.forEach(thread => {
        const methods = threadToMethods.get(thread);
        const methodNames = [...methods.keys()].sort();
        console.log(`=== ${thread} ===`);
        methodNames.forEach(method => console.log(`${method} [${methods.get(method)}]`));
        console.log("");
    })

}


