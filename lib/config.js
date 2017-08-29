module.exports = {
  version: '1.0.5',
  testFramework: {
    defaults: {
      javascript: 'cw-2',
      coffeescript: 'cw-2',
      ruby: 'cw-2',
      python: 'cw-2',
      sql: 'rspec',
      objc: 'cw'
    }
  },
  timeouts: {
    default: 12000,
    go: 15000,
    haskell: 15000,
    sql: 14000,
    java: 20000,
    kotlin: 23000,
    groovy: 23000,
    scala: 27000,
    solidity: 20000,
  },
  moduleRegExs: {
    haskell: /module\s+([A-Z]([a-z|A-Z|0-9]|\.[A-Z])*)\W/,
    julia: /module\s+([a-z|A-Z][a-z|A-Z|0-9]*)\W/,
    erlang: /-module\(([a-z|A-Z][a-z|A-Z|0-9|_]*)\)/,
    elixir: /defmodule\s+([a-z|A-Z][.a-z|A-Z|0-9|_]*)\s+do/,
    objc: /\n*\/\/\s*([a-z|A-Z|0-9|_|-]+)\.m\s*\n/,
    objcHeader: /\n*\/\/\s*([a-z|A-Z|0-9|_|-]+)\.h\s*\n/
  },
  fileExtensions: {
    c: 'c',
    cpp: 'cpp',
    coffeescript: 'coffee',
    crystal: 'cr',
    csharp: 'cs',
    elixir: 'ex',
    erlang: 'erl',
    fsharp: 'fs',
    go: 'go',
    groovy: 'groovy',
    haskell: 'hs',
    java: 'java',
    javascript: 'js',
    julia: 'jl',
    kotlin: 'kt',
    objc: 'm',
    objcHeader: 'h',
    ruby: 'rb',
    scala: 'scala',
    shell: 'sh',
    sql: 'sql',
    swift: 'swift',
  }
};
